## A collection of functions called by server.R
library(dplyr)
library(ggplot2)
library(BayesFactor)

#----------------------------------------------------------------------------
## Simulate datasets with known true proportion 
#----------------------------------------------------------------------------

simulate_data <- function(num_tests, start_date, test_duration,counts , prob_list,
                          alpha_0, beta_0){
  DF <- data.frame( Test_group = numeric(), Date = as.Date(character()), Convert = numeric())
  for (i in 0:num_tests) {
    
    data = data.frame( Test_group = i, 
                       Date = sample(seq(start_date, start_date+ test_duration -1 , by="day"), counts, replace = TRUE ), 
                       Convert = rbinom(n = counts, size= 1, prob = prob_list[i+1]))  
    DF <- rbind(DF, data)
  }
  return(DF)
}
#----------------------------------------------------------------------------
# A function to do data manipulation
#----------------------------------------------------------------------------

transform_data <- function(df ,   # data frame 
                           a = Conf_alpha , st_date = start_date, # confidence level
                           a_0 = alpha_0, b_0= beta_0 # Beta prior parameter
) {
  num_tests = length(unique(df$Test_group))-1 
  result= data.frame()
  #---------start loop ---------------
  for (k in 0:num_tests){
    df_k = df[ df$Test_group == k, ]
    data = as.data.frame.matrix( table(df_k$Date, df_k$Convert))
    data[,1] = data[,1] + data[,2]   # change the first column from not convert to total counts
    
    Date = rownames(data)   #  row name to Date 
    Day = as.numeric( as.Date(Date) - min(as.Date(Date)) +1)  # Compute date from start
    data = cbind(Date, Day, k, data) # add to data
  
    rownames(data) <- NULL
    colnames(data) <- c("Date", "Day", "Test_group", "Total","Convert")
    
    # calculate the cumulated clicked and cumulated converted
    data$Cum_Total  = cumsum(data$Total)
    data$Cum_Convert = cumsum(data$Convert)
    data$CRate  =  data$Cum_Convert/ data$Cum_Total 
    
    ## Upper and lower limit of frequentist confidence interval
    data$Conf_LL =  data$CRate - qnorm(1-a/2, mean = 0, sd = 1) * sqrt( data$CRate*(1-data$CRate)/data$Cum_Total  )
    data$Conf_UL =  data$CRate + qnorm(1-a/2, mean = 0, sd = 1) * sqrt( data$CRate*(1-data$CRate)/data$Cum_Total  )
    data$Conf_LL = as.numeric(lapply(data$Conf_LL, function(x) max(0, x) ))
    data$Conf_UL = as.numeric(lapply(data$Conf_UL, function(x) min(1, x) ))
    
    ## Summaries based on posterior probability 
    post_alpha = a_0 + data$Cum_Convert 
    post_beta  = b_0 + data$Cum_Total - data$Cum_Convert 
    data$Post_mean = (post_alpha)/ ( post_alpha + post_beta ) 
    
    # compute equal-tailed credible interval for the posterior Beta distribution
    data$Cred_LL = qbeta( a/2 , shape1 = post_alpha , shape2 = post_beta ) 
    data$Cred_UL = qbeta(1-a/2, shape1 = post_alpha , shape2 = post_beta )   
    data$Cred_LL = as.numeric(lapply(data$Cred_LL, function(x) max(0, x) ))
    data$Cred_UL = as.numeric(lapply(data$Cred_UL, function(x) min(1, x) ))
    
    # save the data set to result
    if (dim(result)[1] > 0){ result= rbind(result, data)}
    else{result= data}
  }
  #----------End of loop --------------
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
Freq_plot <- function(CR, num_tests=2,  Bayes = TRUE)  
{
  # if Bayes== TRUE, plot Bayesian estimate and Credible Interval 
  if (Bayes== TRUE){column = 'Post_mean'; LL = 'Cred_LL' ; UL = 'Cred_UL'; 
  title= "Bayesian: Posterior Mean and Credible Interval of Proportion Over Time" }
  if (Bayes== FALSE) {column = 'CRate';   LL = 'Conf_LL' ; UL = 'Conf_UL'; 
  title= "Frequentist: Mean and Confidence Interval of Proportion Over Time" }
  
  #-------------------------Set plot color ----------------------------
  cbPalette <- c("#009E73","#0072B2", "#E69F00",   "#D55E00", "#CC79A7","#F0E442","#56B4E9",  "#999999")
  fill_colors = makeTransparent(cbPalette)
  #------------------------Plot settings:---------------------------------
  # compute the upper and lower bound of y-axis to be 20% and 80% quantile of the upper and lower bound
  min_val  = min( quantile( CR$Cred_LL, 0.01 ), quantile( CR$Conf_LL, 0.01 ))
  max_val  = max( quantile( CR$Cred_UL, 0.99 ), quantile( CR$Conf_UL, 0.01 ))
  max_days = quantile(CR$Day, 0.8)  # x-axis position to put legend
  
  #-------------------------------------------------------------------------    
  data = CR[CR[,'Test_group']==0,]       
  p <- plot(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col="red", lty=1 , ylim = c(min_val ,max_val) ,
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

density_plot <- function(alpha_A, beta_A , alpha_B, beta_B , alpha_0, beta_0 ){
  theta<-seq(0,1,0.001) #create theta range from 0 to 1
  prior <- dbeta(theta, alpha_0, beta_0)
  posterior_A <- dbeta(theta, alpha_A, beta_A ) 
  posterior_B <- dbeta(theta, alpha_B, beta_B )
  
  min_prob = min(   min(posterior_A), min(posterior_B))
  max_prob = max( max(posterior_A), max(posterior_B))
  
  # prior 
  prob_plot <- plot(theta, prior,  col="gray", type= 'l', lty=1, lwd = 2, 
                    xlab = 'Proportion', ylab = "Density",   ylim = c(min_prob ,max_prob),
                    main = "Prior and Posterior Densitys")
  #polygon( c(0,  theta, 1),  c( 0,prior,0,  ),  col= makeTransparent("grey") , border=NA)
  
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
                          alpha_0=  alpha_0, beta_0= beta_0,  # set both prior parameters to 1 by default
                          digit =3  , nsim =100000, a = Conf_alpha,
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
    Cred_LL   <- quantile(change , a/2 )
    Cred_UL   <- quantile(change , 1- a/2 )
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
                  round(log(BF), digits = digit),
                  
                  round( p_value, digits=digit ) ) )
  
  colnames(result) = c('Test', 'Users', 'Conversion','Conv Rate (%))', 
                       'Uplift (%)', 'Posterior mean of change','Credible Interval',
                       'Chance of being better(%)',
                       'log Bayes Factor', 
                       'frequentist p-value')
  #-------------------------------------------------------------------
  
  if (density_plot ==TRUE){
    density_plot(alpha_A, beta_A , alpha_B, beta_B, alpha_0, beta_0 )
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
Calculate_change <- function(CR, k , a = Conf_alpha, alpha_0  , 
                             beta_0 , nsim = 10000, digit= 4){
  
  CR0 =  CR[CR[,'Test_group'] ==0, c('Day','Cum_Total','Cum_Convert',"CRate") ]
  CR1 =  CR[CR[,'Test_group'] ==k, c('Day','Cum_Total','Cum_Convert',"CRate") ]
  CR01= merge(CR0, CR1, by= 'Day', all=TRUE)
  
  
  CR01 = cbind( k, CR01)
  colnames(CR01) <- c("Test_group","Day", "Total_0","Convert_0", "CRate_0" ,"Total_k","Convert_k", "CRate_k")    
  
  
  N= dim(CR0)[1]  # total number of days in the study
  
  n0 = CR01$Total_0; x0 = CR01$Convert_0; p0 =  CR01$CRate_0 
  n1 = CR01$Total_k; x1 = CR01$Convert_k; p1 =  CR01$CRate_k 
  
  
  p_hat = (x0+ x1) /(n0+n1) # pooled estimate of proportion, assuming two groups have equal proportion
  
  ## ----------Upper and lower limit of frequentist confidence interval------------
  z = qnorm(1-a/2, mean = 0, sd = 1) 
  
  CR01$CRate_change = p1- p0
  CR01$Conf_LL =  (p1- p0) - z* sqrt( p_hat*(1-p_hat) *(1/n0 + 1/n1) )
  CR01$Conf_UL =  (p1- p0) + z* sqrt( p_hat*(1-p_hat) *(1/n0 + 1/n1) )
  
  Z_stat = (p1- p0)/ sqrt( p_hat*(1-p_hat) *(1/n0 + 1/n1) )
  # Use normal approximation to calculate p-value
  p_value = (1- pnorm(abs(Z_stat)) )*2 
  
  #--------------------------------------------------------------------------------
  # For each day, simulate posterior distribution of difference p1-p0:
  # And compute Upper and lower limit of Bayesian credible interval----------------
  
  Post_mean <- c()
  Cred_LL   <- c()
  Cred_UL   <- c()
  logBF     <- c()
  prob_better <- c()
  
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
    
    prob_better <- c(prob_better, prob_B_beats_A(alpha_A, beta_A, alpha_B, beta_B)*100 )
    
    #-----------Bayes Factors -------------------------------------------
    mat = matrix(c(x0[i], n0[i]-x0[i], x1[i], n1[i]-x1[i]), nrow = 2 ) 
    logBF = c( logBF, log(as.numeric(as.vector( contingencyTableBF( mat , sampleType = "indepMulti", 
                                                                    fixedMargin = "cols")))) )
  }
  
  CR01$Post_mean = Post_mean
  CR01$Cred_LL   = Cred_LL
  CR01$Cred_UL   = Cred_UL
  CR01$prob_better= prob_better
  CR01$Uplift    = (p1 - p0)/p0 *100
  CR01$logBF     = logBF
  CR01$p_value   = p_value
  #--------------------------------------------------------------------------------
  CR01 <- CR01[ -c(5,8) ]
  return(CR01)
}


#----------------------------------------------------------------------------
# Apply the functionn to 3 test groups.
#----------------------------------------------------------------------------
Cal_all_change <- function(CR , Conf_alpha, alpha_0, beta_0 ){
  num_tests = length(unique( CR[,'Test_group'])) -1 
  result= data.frame()
  for (k in 1:num_tests){
    data = Calculate_change(CR, k , Conf_alpha, alpha_0, beta_0 )
    # save the data set to result
    if (dim(result)[1] > 0){ result= rbind(result, data)}
    else{result= data}
  }
  return(result)
}
#----------------------------------------------------------------------------
### 3.3 Plot the point estimates of change in conversion rate, and thier credible interval over time
#----------------------------------------------------------------------------
Change_plot <- function(CR_change, Bayes = TRUE)  
{
  num_tests = length(unique( CR_change[,'Test_group'])) 
  
  # if Bayes== TRUE, plot Bayesian estimate and Credible Interval 
  if (Bayes== TRUE){column = 'Post_mean'; LL = 'Cred_LL' ; UL = 'Cred_UL'; 
  title= "Bayesian: Posterior Mean and Credible Interval of Change Over Time" }
  else{        column = 'CRate_change';   LL = 'Conf_LL' ; UL = 'Conf_UL'; 
  title= "Frequentist: Mean and Confidence Interval of Change Over Time" }
  
  #-------------- Set color for plot ------------------------------
  cbPalette <- c("#009E73","#0072B2", "#E69F00",   "#D55E00", "#CC79A7","#F0E442","#56B4E9",  "#999999")
  fill_colors = makeTransparent(cbPalette)
  #-------------------------------------------------------------------------
  # Plot settings:
  # compute the upper and lower bound of y-axis to be 20% and 80% quantile of the upper and lower bound
  min_val = min( quantile(CR_change[, 'Cred_LL'], 0.01 ), quantile(CR_change[, 'Conf_LL'], 0.01 ))
  max_val = max( quantile(CR_change[, 'Cred_UL'], 0.99 ), quantile(CR_change[, 'Conf_UL'], 0.01 ))
  max_days = quantile(CR_change[,'Day'], 0.8)  # x-axis position to put legend
  
  # -------------- plot the first group -------------- 
  data =  CR_change[CR_change[,'Test_group'] == 1, ] 
  
  plot(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col= cbPalette[1], lty=1 , ylim = c(min_val, max_val) ,
         main = title, 
         xlab = 'Days after tests start' , ylab= 'Proportion')
  polygon( c(data[,'Day'] , rev(data[,'Day']) ), c(data[, LL] , rev(data[, UL]) ), 
           col= fill_colors[1], border=NA)
  
  # -------------- plot the other groups-------------- 
  for (k in 2:num_tests){
    data = CR_change[CR_change[,'Test_group'] == k, ]
    lines(  data[,'Day'],  data[, column] ,type = "l", lwd = 3, col= cbPalette[k], lty=k+1   )
    polygon( c(data[,'Day'] , rev(data[,'Day']) ), c(data[,LL] , rev(data[, UL]) ), 
             col= fill_colors[k], border=NA)
  }
  abline(h=0)
  
  #------------------add legend to the plot -----------------
  legend_list = c()
  for (k in 1:num_tests){ legend_list = c(legend_list,  paste0("Test ", k) )}
  legend( max_days , max_val, legend= legend_list, 
          col=c( cbPalette[1:k]), lty=1:(k+1), cex=0.8, title="Test group")
  
}

#----------------------------------------------------------------------------
### ### Plot different summaries over time
# 1. The Bayes factor
# 2. p-value
# 2. The uplift 
# 3. Probability of each test being better than the default 
#----------------------------------------------------------------------------

plot_change_column <- function(CR_change, num_tests=3, variable, var_label, 
                               hline= 0, plot_min_pct=0.01, plot_max_pct= 0.90,
                               legend_position= "right",  y_max= 30 ){
  #-------------- Set color for plot ------------------------------
  cbPalette <- c("#009E73","#0072B2", "#E69F00",   "#D55E00", "#CC79A7","#F0E442","#56B4E9",  "#999999")
  fill_colors = makeTransparent(cbPalette)
  #-------------------------------------------------------------------------
  # Plot settings:
  # compute the upper and lower bound of y-axis to be 20% and 80% quantile of the upper and lower bound
  min_val = quantile(CR_change[, variable], plot_min_pct )
  max_val = quantile(CR_change[, variable], plot_max_pct ) 
  print(min_val, max_val)
  # -------------- plot the first group -------------- 
  data =  CR_change[CR_change[,'Test_group'] == 1, ] 
  plot(  data[,'Day'],  data[, variable] ,type = "l", lwd = 3, 
         col= cbPalette[1], lty=1 , ylim = c(min_val, max_val) ,
         main = paste0("Trend of ", var_label, " Over Time"), 
         xlab = var_label , ylab= 'Percent')
  
  # -------------- plot the other groups-------------- 
  for (k in 2:num_tests){
    data =  CR_change[CR_change[,'Test_group'] == k, ] 
    lines(  data[,'Day'],  data[, variable] ,type = "l", lwd = 3, col= cbPalette[k], lty=k+1   )
  }
  abline(h= hline)
  
  #------------------add legend to the plot -----------------
  legend_list = c()
  for (k in 1:num_tests){ legend_list = c(legend_list,  paste0("Test ", k) )}
  legend( legend_position , legend= legend_list, 
          col=c(  cbPalette[1:k]), lty=1:(k+1), cex=0.8, title="Test group")
  
  #-------------------------------------------------------------------------
}

#----------------------------------------------------------------------------



#----------------------------------------------------------------------------



#----------------------------------------------------------------------------



#----------------------------------------------------------------------------



#----------------------------------------------------------------------------




#----------------------------------------------------------------------------
