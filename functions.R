## A collection of functions called by server.R

# A function to do data manipulation

transform_data <- function(df ,   # data frame 
                           a = alpha ,  # confidence level
                           a_0 = alpha_0, b_0= beta_0 # Beta prior parameter
) {
  result= data.frame()
  for (k in 0:num_tests){
    df_k = df[ df$Test_group == k, ]
    data = table(df_k$Date, df_k$Convert)
    data[,1] = data[,1] + data[,2]
    
    # calculate the cumulated clicked and cumulated converted
    CumTot  = cumsum(data[,1])
    CumConv = cumsum(data[,2])
    p  =  CumConv/ CumTot 
    
    ## Upper and lower limit of frequentist confidence interval
    Conf_LL =  p - qnorm(1-a/2, mean = 0, sd = 1) * sqrt( p*(1-p)/CumTot  )
    Conf_UL =  p + qnorm(1-a/2, mean = 0, sd = 1) * sqrt( p*(1-p)/CumTot  )
    
    ## Summaries based on posterior probability 
    post_alpha = a_0 + CumConv 
    post_beta  = b_0 + CumTot - CumConv 
    post_mean = (post_alpha)/ ( post_alpha + post_beta ) 
    # compute equal-tailed credible interval for the posterior Beta distribution
    Cred_LL = qbeta( a/2 , shape1 = post_alpha , shape2 = post_beta ) 
    Cred_UL = qbeta(1-a/2, shape1 = post_alpha , shape2 = post_beta )    
    
    data = cbind(Date = as.Date(rownames(data)), as.Date(rownames(data)) - start_date +1, df_k[1,'Test_group'], 
                 data, CumTot  , CumConv, p , Conf_LL, Conf_UL , post_mean, Cred_LL, Cred_UL )
    
    # save the data set to result
    if (dim(result)[1] > 0){ result= rbind(result, data)}
    else{result= data}
  }
  colnames(result)<- c("Date","Day", "Test_group", "Total","Convert", "Cum_Total", "Cum_Convert", "CRate", 
                       "Conf_LL","Conf_UL" , "Post_mean", "Cred_LL", "Cred_UL")
  return(result)
}

#----------------------------------------------------------------------------
# helper function to make colors transparent: 
makeTransparent = function(..., alpha=0.15) {
  # A function to make colors transparent: 
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#----------------------------------------------------------------------------
# The main function to plot point estimate and confidence region, 
# with the option of using Bayesian or frequentist methods:

Freq_plot <- function(df, num_tests=2, 
                      Bayes = TRUE)  
{
  # if Bayes== TRUE, plot Bayesian estimate and Credible Interval 
  if (Bayes== TRUE){column = 'Post_mean'; LL = 'Cred_LL' ; UL = 'Cred_UL'; 
  title= "Bayesian: Posterior Mean and Credible Interval of Proportion Over Time" }
  if (Bayes== FALSE) {column = 'CRate';   LL = 'Conf_LL' ; UL = 'Conf_UL'; 
  title= "Frequentist: Mean and Confidence Interval of Proportion Over Time" }
  #-------------------------------------------------------------------------
  cbPalette <- c("#009E73","#0072B2", "#E69F00",   "#D55E00", "#CC79A7","#F0E442","#56B4E9",  "#999999")
  fill_colors = makeTransparent(cbPalette)
  #-------------------------------------------------------------------------
  # plot the default group
  data = CR[CR[,'Test_group']==0,]   
  
  plot(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col="red", lty=1 , ylim = c(0,0.07) ,
         main = title, 
         xlab = 'Days after tests start' , ylab= 'Proportion')
  polygon( c(data[,'Day'] , rev(data[,'Day']) ), c(data[, LL] , rev(data[, UL]) ), 
           col=rgb(1, 0, 0,0.1), border=NA)
  #-------------------------------------------------------------------------    
  abline(h=0)
  # plot the rest test groups
  for (k in 1:num_tests){
    data = CR[CR[,'Test_group']==k,]
    lines(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col= cbPalette[k], lty=k+1   )
    polygon( c(data[,'Day'] , rev(data[,'Day']) ), c(data[,LL] , rev(data[, UL]) ), 
             col= fill_colors[k], border=NA)
  }
  legend(50,  0.06, legend=c("Default", "Test 1", "Test 2"),
         col=c("red", cbPalette[1:k]), lty=1:(k), cex=0.8, title="Test group")
}
#----------------------------------------------------------------------------
# A helper function to compute the probability that Test B is better than default Test A. 
# A function modified from fomulas on http://www.evanmiller.org/bayesian-ab-testing.html 
prob_B_beats_A = function(alpha_A, beta_A, alpha_B, beta_B){
  total = 0.0
  for (i in 0:(alpha_B-1) ){
    total = total+  exp(lbeta(alpha_A+i, beta_B+ beta_A) - log(beta_B+i) - lbeta(1+i, beta_B) - lbeta(alpha_A, beta_A))
  }
  return (total)    
}

# And another helper function for A/B/C testing.
# A/B/C testing: binary outcomes
probability_C_beats_A_and_B <- function(alpha_A, beta_A, alpha_B, beta_B, alpha_C, beta_C){
  total = 0.0
  for (i in 0:(alpha_A-1) ){
    for (j in 0:(alpha_B-1)){
      total = total + exp(lbeta(alpha_C+i+j, alpha_A+alpha_B+alpha_C) - log(alpha_A+i) - log(alpha_B+j))
    }        
  }         
  return (1 - probability_B_beats_A(alpha_C, alpha_C, alpha_A, alpha_A)  - probability_B_beats_A(alpha_C, alpha_C, alpha_B, alpha_B) + total )    
}

#----------------------------------------------------------------------------
# A function to do Bayesian A/B testing, which will provide a summary table 
# and have the option to produce density plots and a bar char of probabilities of at test better than the other.

# -------------make density plot -----------------#
density_plot <- function(alpha_A, beta_A , alpha_B, beta_B ){
    theta<-seq(0,1,0.001) #create theta range from 0 to 1
    prior <- dbeta(theta, 1,1)
    posterior_A <- dbeta(theta, alpha_A, beta_A )
    posterior_B <- dbeta(theta, alpha_B, beta_B )
    
    
    prob_plot <- plot(theta, prior,  col="gray", lty=2, xlab = 'prop', ylab = "Density",   ylim = c(0,30),
    main = "Prior and Posterior Densitys")
    lines(theta, posterior_A, lwd = 3, col="dodgerblue", lty=3)
    lines(theta, posterior_B, lwd = 3, col="orange", lty=3)
    legend("topright",lwd=3,
    c("prior", "posterior_A","posterior_B"),
    col = c( "grey","dodgerblue","orange")
    )
}

# ------------- Best probability Bar chart -----------------#
bestProb_plot <- function(best_A, best_B){
    names <-c("A", "B")
    prob_list = c(best_A, best_B)
    yy <- barplot(prob_list ,main="Chance of B outperforming A", width = 1, horiz=TRUE,names.arg=names,las=1,
    xlab = "Percent")
    ## Add text at top of bars
    text(y = yy,  x = prob_list, label = prob_list, pos = 3, cex = 0.8)
    
}

Bayes_AB_test <- function(nA, xA, nB, xB, make_plot=TRUE,
alpha0= 1, beta_0= 1){  # set both prior parameters to 1 by default
    # create an empty data frame:
    result= data.frame(Test= character(), Users = integer(), Conversion= integer(), CR=double(),
    Uplift= double(), Chance_of_being_best=double() )
    
    CR_A = xA/nA*100
    CR_B = xB/nB*100
    uplift_B = (CR_B- CR_A)/CR_A *100
    
    alpha_A = alpha_0 + xA
    alpha_B = alpha_0 + xB
    beta_A  = beta_0 + nA-xA
    beta_B  = beta_0 + nB-xB
    
    best_B = 100*prob_B_beats_A (alpha_A, beta_A , alpha_B, beta_B )
    best_A = 100- best_B
    
    result= rbind(c("A", nA, xA, CR_A, NA, best_A) ,
    c("B", nB, xB, CR_B, uplift_B, best_B))
    colnames(result) = c('Test', 'Users', 'Conversion','CR (%))', 'Uplift (%)', 'Chance of being best (%)')
    if (make_plot ==TRUE){
        par(mfrow = c(2, 1))
        
        density_plot(alpha_A, beta_A , alpha_B, beta_B )
        bestProb_plot(best_A, best_B )
    }
    
    return (result)
}

#----------------------------------------------------------------------------
# Bayesian A/B testing summary for change of conversion rate:
Calculate_change <- function(df, k , a = alpha, nsim = 10000){
  
  CR0 =  CR[CR[,'Test_group'] ==0, c('Day','Cum_Total','Cum_Convert',"CRate") ]
  CR1 =  CR[CR[,'Test_group'] ==k, c('Cum_Total','Cum_Convert',"CRate") ]
  CR01= merge(CR0, CR1, by=0, all=TRUE)
  
  N= dim(CR0)[1]  # total number of days in the study
  
  n0 = CR01[,3]; x0 = CR01[,4]; p0 =  CR01[,5] 
  n1 = CR01[,6]; x1 = CR01[,7]; p1 =  CR01[,8]  
  
  ## ----------Upper and lower limit of frequentist confidence interval------------
  z = qnorm(1-a/2, mean = 0, sd = 1) 
  Conf_LL =  (p1- p0) - z* sqrt( p0*(1-p0)/n0 + p1*(1-p1)/n1  )
  Conf_UL =  (p1- p0) + z* sqrt( p0*(1-p0)/n0 + p1*(1-p1)/n1  )
  #--------------------------------------------------------------------------------
  # For each day, simulate posterior distribution of difference p1-p0:
  # And compute Upper and lower limit of Bayesian credible interval----------------
  
  Post_mean <- c()
  Cred_LL   <- c()
  Cred_UL   <- c()
  for (i in 1:N){
    # posterior dist parameters: 
    alpha_A = alpha_0 + x0[i]
    alpha_B = alpha_0 + x1[i]  
    beta_A  = beta_0 + n0[i]-x0[i]
    beta_B  = beta_0 + n1[i]-x1[i]
    # simulate theta0 and theta1
    theta0 = rbeta(n= nsim, shape1 = alpha_A, shape2 =beta_A)
    theta1 = rbeta(n= nsim, shape1 = alpha_B, shape2 =beta_B) 
    # compute the difference
    change = theta1 - theta0
    # compute mean, sd, approximate 1-alpha Credible Interval 
    mean_change = mean(change)
    sd_change   = sd(change)
    Post_mean = c(Post_mean, mean_change)
    Cred_LL   <- c(Cred_LL, mean_change - z * sd_change)
    Cred_UL   <- c(Cred_UL, mean_change + z * sd_change)
  }
  
  
  # -------------------Chance of being better than default, Uplift---------------------
  
  prob_better <- c()
  Uplift <- c()
  for (i in 1:N){
    prob_better <- c(prob_better, as.numeric(Bayes_AB_test(nA= n0[i], xA=x0[i], 
                                                           nB=n1[i], xB=x1[i], make_plot= FALSE)[2,6]) )
    Uplift     <- c(Uplift    , as.numeric(Bayes_AB_test(nA= n0[i], xA=x0[i], 
                                                         nB=n1[i], xB=x1[i], make_plot= FALSE)[2,5]) )    }
  #--------------------------------------------------------------------------------
  CR01 = cbind(CR01, p1-p0 ,Conf_LL, Conf_UL , Post_mean, Cred_LL, Cred_UL, prob_better, Uplift)
  colnames(CR01) = c("Date","Day", "Total_0","Convert_0", "CRate_0", "Total_1","Convert_1", "CRate_1", 
                     'CRate_change',"Conf_LL", "Conf_UL", "Post_mean", "Cred_LL","Cred_UL",
                     "Prob_better", "Uplift_pct")
  return(CR01)
}

#----------------------------------------------------------------------------
### 3.3 Plot the point estimates of change in conversion rate, and thier credible interval over time
Change_plot <- function( num_tests=3,   Bayes = TRUE)  
{
  # if Bayes== TRUE, plot Bayesian estimate and Credible Interval 
  if (Bayes== TRUE){column = 'Post_mean'; LL = 'Cred_LL' ; UL = 'Cred_UL'; 
  title= "Bayesian: Posterior Mean and Credible Interval of Change Over Time" }
  else{        column = 'CRate_change';   LL = 'Conf_LL' ; UL = 'Conf_UL'; 
  title= "Frequentist: Mean and Confidence Interval of Change Over Time" }
  #-------------------------------------------------------------------------
  cbPalette <- c("#009E73","#0072B2", "#E69F00",   "#D55E00", "#CC79A7","#F0E442","#56B4E9",  "#999999")
  fill_colors = makeTransparent(cbPalette)
  
  # plot the default group
  data =  CR01  
  
  plot(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col="red", lty=1 , ylim = c(-0.03, 0.06) ,
         main = title, 
         xlab = 'Days after tests start' , ylab= 'Proportion')
  polygon( c(data[,'Day'] , rev(data[,'Day']) ), c(data[, LL] , rev(data[, UL]) ), 
           col=rgb(1, 0, 0,0.1), border=NA)
  
  # plot the rest test groups
  for (k in 2:num_tests){
    data = get(paste0("CR0", k))
    lines(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col= cbPalette[k], lty=k+1   )
    polygon( c(data[,'Day'] , rev(data[,'Day']) ), c(data[,LL] , rev(data[, UL]) ), 
             col= fill_colors[k], border=NA)
  }
  abline(h=0)
  
  legend(50,  0.045, legend=c("Test 1", "Test 2", "A/A Test"),
         col=c("red", cbPalette[2:k]), lty=1:(k+1), cex=0.8, title="Test group")
  
}




#----------------------------------------------------------------------------
### 3.3.3 Plot (a) the uplift and (b) probability of each test being better than the default over time
plot_Uplift <- function( num_tests=3)  
{
  #-------------------------------------------------------------------------
  cbPalette <- c("#009E73","#0072B2", "#E69F00",   "#D55E00", "#CC79A7","#F0E442","#56B4E9",  "#999999")
  fill_colors = makeTransparent(cbPalette)
  
  #par(mfrow = c(1, 2))
  #--------------------------------------------------------------------------
  column ="Uplift_pct"
  title ="Uplift percentage over time"
  # plot the default group
  data =  CR01  
  plot(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col="red", lty=1 , ylim = c(-20, 120) ,
         main = title, 
         xlab = 'Days after tests start' , ylab= 'Percent')
  
  # plot the rest test groups
  for (k in 2:num_tests){
    data = get(paste0("CR0", k))
    lines(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col= cbPalette[k], lty=k+1   )
  }
  abline(h=50)
  
  legend(50,  80, legend=c("Test 1", "Test 2", "A/A Test"),
         col=c("red", cbPalette[2:k]), lty=1:(k+1), cex=0.8, title="Test group")
  #--------------------------------------------------------------------------
  column ="Prob_better"
  title ="Probability (%) of better than default over time"
  # plot the default group
  data =  CR01  
  plot(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col="red", lty=1 , ylim = c(-20, 120) ,
         main = title, 
         xlab = 'Days after tests start' , ylab= 'Percent')
  
  # plot the rest test groups
  for (k in 2:num_tests){
    data = get(paste0("CR0", k))
    lines(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col= cbPalette[k], lty=k+1   )
  }
  abline(h=50)
  
  legend(50,  80, legend=c("Test 1", "Test 2", "A/A Test"),
         col=c("red", cbPalette[2:k]), lty=1:(k+1), cex=0.8, title="Test group")
  
  
  
}


#----------------------------------------------------------------------------



#----------------------------------------------------------------------------



#----------------------------------------------------------------------------



#----------------------------------------------------------------------------



#----------------------------------------------------------------------------




#----------------------------------------------------------------------------