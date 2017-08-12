## A collection of functions called by server.R
library(dplyr)
library(ggplot2)
library(BayesFactor)

#----------------------------------------------------------------------------
## Simulate datasets with known true proportion 
#----------------------------------------------------------------------------

simulate_data <- function(num_tests, start_date, test_duration,counts , prob_list ){
  for (i in 0:num_tests) {
    assign(paste0("DF", i), data.frame(
      Test_group = i, 
      Date = sample(seq(start_date, start_date+ test_duration -1 , by="day"), counts, replace = TRUE ), 
      Convert = rbinom(n = counts, size= 1, prob = prob_list[i+1]))  
    )
  }
  
  # Concatenate into one long dataset, on row corresponding to a users
  DF = DF0
  for ( k in 1:num_tests) {
    DF= rbind(DF, get(paste0("DF", k)) )} 
  return(DF)
}


#----------------------------------------------------------------------------
# A function to do data manipulation
#----------------------------------------------------------------------------

transform_data <- function(df ,   # data frame 
                           a = alpha ,  # confidence level
                           a_0 = alpha_0, b_0= beta_0 # Beta prior parameter
) {
  num_tests = length(unique(DF$Test_group))-1 
  
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
                 data, CumTot  , CumConv, p , Conf_LL, Conf_UL , post_mean, Cred_LL, Cred_UL  )
    
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
#----------------------------------------------------------------------------

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
#----------------------------------------------------------------------------
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
  # Plot settings:
  # compute the upper and lower bound of y-axis to be 20% and 80% quantile of the upper and lower bound
  min_val = min( quantile(df[, 'Cred_LL'], 0.01 ), quantile(df[, 'Conf_LL'], 0.01 ))
  max_val = max( quantile(df[, 'Cred_UL'], 0.99 ), quantile(df[, 'Conf_UL'], 0.01 ))
  max_days = quantile(df[,'Day'], 0.8)  # x-axis position to put legend
  
  #-------------------------------------------------------------------------    
  data = CR[CR[,'Test_group']==0,]       
  p<- plot(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col="red", lty=1 , ylim = c(min_val ,max_val) ,
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
  #-------------------------------------------------------------------------        
  # add legend to the plot
  legend_list = c()
  for (k in 1:num_tests){ legend_list = c(legend_list,  paste0("Test ", k) )}
  legend( max_days , max_val, legend= legend_list, 
          col=c("red", cbPalette[2:k]), lty=1:(k+1), cex=0.8, title="Test group")
}

#----------------------------------------------------------------------------
# A helper function to compute the probability that Test B is better than default Test A. 
# A function modified from fomulas on http://www.evanmiller.org/bayesian-ab-testing.html 
#----------------------------------------------------------------------------
prob_B_beats_A = function(alpha_A, beta_A, alpha_B, beta_B){
  total = 0.0
  for (i in 0:(alpha_B-1)){
    total = total + exp(lbeta(alpha_A + i, beta_B + beta_A) 
                        - log(beta_B + i) - lbeta(1 + i, beta_B) - lbeta(alpha_A, beta_A))
  }
  return(total)
}

#----------------------------------------------------------------------------
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
# -------------prior and posterior density plot -----------------#
#----------------------------------------------------------------------------
density_plot <- function(alpha_A, beta_A , alpha_B, beta_B ){
  theta<-seq(0,1,0.001) #create theta range from 0 to 1
  prior <- dbeta(theta, 1,1)
  posterior_A <- dbeta(theta, alpha_A, beta_A ) 
  posterior_B <- dbeta(theta, alpha_B, beta_B )
  
  min_prob = min(min(prior) , min(posterior_A), min(posterior_B))
  max_prob = max(max(prior) , max(posterior_A), max(posterior_B))
  
  # prior 
  prob_plot <- plot(theta, prior,  col="gray", type= 'l', lty=1, lwd = 2, 
                    xlab = 'Proportion', ylab = "Density",   ylim = c(min_prob ,max_prob),
                    main = "Prior and Posterior Densitys")
  polygon( c(0 , theta,1),  c(0,prior,0),  col= makeTransparent("grey") , border=NA)
  
  # posterior of theta_A , theta_B
  lines(theta, posterior_A, lwd = 2, col="dodgerblue", lty=1)
  polygon( theta  ,  posterior_A,  col= makeTransparent("dodgerblue") , border=NA)
  
  lines(theta, posterior_B, lwd = 2, col="orange", lty=1)
  polygon( theta  ,  posterior_B,  col= makeTransparent("orange") , border=NA)
  
  # Add posterior mean
  abline( v = alpha_A/(alpha_A + beta_A), lty= 1, lwd=2, col= makeTransparent("dodgerblue") )
  abline( v = alpha_B/(alpha_B + beta_B) , lty= 1, lwd=2, col= makeTransparent("orange") )
  
  # Add legend to plot:
  legend("topright",lwd=2, lty= 1 ,
         legend= c("prior", "posterior_A","posterior_B"),
         col = c( "grey","dodgerblue","orange")
  )    
}
#----------------------------------------------------------------------------
# ------------- Best probability Bar chart -----------------#
#----------------------------------------------------------------------------
bestProb_plot <- function(best_A, best_B){
  names <-c("A", "B")
  prob_list = c(best_A, best_B)
  yy <- barplot(prob_list ,main="Chance of B outperforming A", width = 1, horiz=TRUE,names.arg=names,las=1, 
                xlab = "Percent")
  ## Add text at top of bars
  text(y = yy,  x = prob_list, label = prob_list, pos = 2, cex = 0.8)
  
}

#----------------------------------------------------------------------------
# -------------Plot posterior density of change -----------------#
#----------------------------------------------------------------------------
posterior_plot <- function(change, mean_change, Cred_LL, Cred_UL){
  d = density(change )
  plot(d, main="Posterior Density of proportion change", col = "#009E73" )
  polygon(d, col='#009E7326', border=NA)
  abline( v = Cred_LL , lty= 3, lwd=2, col= "grey")
  abline( v = Cred_UL , lty= 3, lwd=2, col= "grey")
  abline( v = mean_change , lty= 1, lwd=2, col= "grey")
  
  # add legend 
  legend("topright",lwd=2, lty= c(1,3),
         legend= c("Posterior mean", "Credible Interval"),
         col = c( "grey","grey"),  cex = 0.8
  )  
  ## Add mean and CI value
  text(y = 0,  x = mean_change, label = round(mean_change, digits=4) , cex = 0.8)
  text(y = 0,  x = Cred_LL, label = round(Cred_LL, digits=4) , cex = 0.8)
  text(y = 0,  x = Cred_UL, label = round(Cred_UL, digits=4) , cex = 0.8)
}        

#----------------------------------------------------------------------------
# A function to do Bayesian A/B testing, which will provide a summary table 
# and have the option to produce density plots and a bar char of probabilities of at test better than the other.
#----------------------------------------------------------------------------
Bayes_AB_test <- function(nA, xA, nB, xB, 
                          out_data = TRUE, diff_plot= FALSE, 
                          bestProb_plot= FALSE, density_plot=FALSE, 
                          alpha_0= 1, beta_0= 1,  # set both prior parameters to 1 by default
                          digit =3  , nsim =100000, alpha = 0.1,
                          calculate_CI = TRUE  ){ 
  
  # Conversion rate and uplift
  CR_A = xA/nA*100
  CR_B = xB/nB*100   
  uplift_B = (CR_B- CR_A)/CR_A *100
  
  #-------------------------------------------------------------------        
  # The posterior beta-distritubion of theta_A and theta_B
  alpha_A = alpha_0 + xA
  alpha_B = alpha_0 + xB  
  beta_A  = beta_0 + nA-xA
  beta_B  = beta_0 + nB-xB
  
  #-------------------------------------------------------------------   
  # If we want to get credible interval of change, need to do simulation, 
  # asumming the two proportions are independent:
  if (calculate_CI == TRUE){
    # simulate theta0 and theta1
    theta0 = rbeta(n= nsim, shape1 = alpha_A, shape2 =beta_A)
    theta1 = rbeta(n= nsim, shape1 = alpha_B, shape2 =beta_B) 
    # compute the difference
    change = theta1 - theta0
    
    # mean_change = mean(change)
    # assuming indepence, the theoretical expectation of change is:
    mean_change = alpha_B/(alpha_B + beta_B) - alpha_A/(alpha_A + beta_A)
    
    # compute equal-tailed (1-alpha) Credible Interval         
    Cred_LL   <- quantile(change , alpha/2 )
    Cred_UL   <- quantile(change , 1- alpha/2 )
  }
  else {Cred_LL = NA; Cred_UL = NA}
  
  #-------------------------------------------------------------------    
  # compute frequentis p-value using Fisher's exact method: 
  mat = matrix(c(xA, nA-xA, xB, nB-xB), nrow = 2 ) 
  p_value = fisher.test( mat , alternative = "two.sided")$p.value
  
  # Compute Bayese Factor using a function from BayesFactor library: 
  BF = as.numeric(as.vector( contingencyTableBF( mat , sampleType = "indepMulti", fixedMargin = "cols")))
  
  # Probablity of being better can be derived from BF: 
  best_B = prob_B_beats_A(alpha_A, beta_A, alpha_B, beta_B) *100
  best_A = 100 - best_B
  
  #-------------------------------------------------------------------
  # combine and save to result
  result= rbind(c("A", nA, xA, round(CR_A, digits = digit) , 
                  NA, NA, NA, round(best_A, digits = digit),  NA, NA ) ,
                
                c("B", nB, xB, round(CR_B, digits = digit) ,  
                  round(uplift_B, digits = digit), 
                  round(mean_change, digits= digit),
                  paste0("(", round(max(0,Cred_LL*100), digits= digit), ", ", 
                         round(min( Cred_UL*100, 100), digits = digit), ")"), 
                  round(best_B, digits = digit), 
                  round(BF, digits = digit),
                  
                  round( p_value, digits=digit ) ) )
  
  colnames(result) = c('Test', 'Users', 'Conversion','Conv Rate (%))', 
                       'Uplift (%)', 'Posterior mean of change','Credible Interval',
                       'Chance of being better(%)',
                       'Bayes Factor', 
                       'frequentist p-value')
  #-------------------------------------------------------------------
  
  if (density_plot ==TRUE){
    density_plot(alpha_A, beta_A , alpha_B, beta_B )
  }
  if (bestProb_plot ==TRUE){
    bestProb_plot(round(best_A, digits= digit) , round(best_B, digits= digit) )
  }
  if (calculate_CI == TRUE & diff_plot == TRUE ){
    posterior_plot(change, mean_change, Cred_LL, Cred_UL)
  }
  if(out_data ==TRUE) {return (result)}
  
}

#----------------------------------------------------------------------------
# Bayesian A/B testing summary for change of conversion rate:
#----------------------------------------------------------------------------
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
