#-------------------------------------------------------------------------
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
#-------------------------------------------------------------------------

library(shinydashboard)
library(mlxR)
library('shiny')
library('ggplot2')
source("functions.R")
#------------------------------------------------------------------------------------

shinyServer(function(input, output){


  #-------------------------------------------------------------------------------
  # -------------------------single AB_test resulst table ------------------------- 
  #-------------------------------------------------------------------------------
    
    output$single_ABtest <- renderTable({
      result <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                                alpha_0 = input$alpha_0, beta_0= input$beta_0 , 
                               a= as.numeric(input$Conf_alpha) )
        return(result)
    })
    
    #-------------------------------------------------------------------------------
    # A/B Test density plot, posterior plot of change, plot of probability being better; 
    #-------------------------------------------------------------------------------
    output$ABtest_density <- renderPlot({
      density_plot <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                               alpha_0 = input$alpha_0, beta_0= input$beta_0, out_data = FALSE, 
                               density_plot =TRUE , a = as.numeric(input$Conf_alpha) )
      return(density_plot)
    })
    
    output$ABtest_change_density <- renderPlot({
      density_plot <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                                     alpha_0 = as.numeric(input$alpha_0), 
                                     beta_0 = input$beta_0, out_data = FALSE, 
                                     diff_plot =TRUE, a = as.numeric(input$Conf_alpha)  )
      return(density_plot)
    })    
    
    output$ABtest_bestProb <- renderPlot({
      bestProb_plot <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                                     alpha_0 = input$alpha_0, beta_0= input$beta_0, out_data = FALSE, 
                                     bestProb_plot =TRUE , a = as.numeric(input$Conf_alpha)  )
      return(bestProb_plot)
    })   
    
    #-------------------------------------------------------------------------------
    # calculate point estimate and CI for conversion rate by group over time 
    #-------------------------------------------------------------------------------
    #data <- reactiveValues(sim_data = NULL)

    sim_data <- reactive({
      raw_prob_list = c(input$pA,input$pB, input$pC)/100
      num_test = sum ( raw_prob_list > 0  )
      prob_list = raw_prob_list[raw_prob_list >0 ]
      Test_name = c('A', 'B','C')[raw_prob_list >0 ]
      
      # simulate raw data 
      df = simulate_data( num_test-1 , input$start_date, input$test_duration,
                         input$counts , prob_list, input$alpha_0_sim, input$beta_0_sim)

      # aggregate counts and compute group rate statistics 
      new_df <- transform_data( df,   # data frame 
                                a = as.numeric(input$Conf_alpha) ,  # confidence level
                                a_0 = input$alpha_0, b_0= input$beta_0) # Beta prior parameter
      return(new_df)
    })
    #-------------------------------------------------------------------------------
    # save as an output table 
    output$sim_data <- renderTable({ return( sim_data() ) })
    
    #-------------------------------------------------------------------------------
    # calculate change of conversion rate compared with default, point estimate, CI
    #-------------------------------------------------------------------------------
    change_data <- reactive({
       CR_change = Cal_all_change(CR = sim_data() ,  
                             Conf_alpha = as.numeric(input$Conf_alpha) ,
                             alpha_0 = input$alpha_0_sim, beta_0= input$beta_0_sim ) 
       return(CR_change) 
    })
    
    #-------------------------------------------------------------------------------
    # save as an output table     
    output$Conv_change <- renderTable({
      return( change_data() )
    })

    #-------------------------------------------------------------------------------
    output$plot1 <- renderPlot({
      data = as.data.frame(change_data())
      p = ggplot(data, aes(Day, logBF,colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      return(p)
    })
    output$plot2 <- renderPlot({
      data = as.data.frame(change_data())
      p = ggplot(data, aes(Day, p_value,colour= as.factor(Test_group) ))  +  
                   geom_point() +  geom_line() + theme(legend.position="bottom")
      return(p)
    })
    output$plot3 <- renderPlot({
      data = as.data.frame(change_data())
      p = ggplot(data, aes(Day, Uplift,colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      return(p)
    })
    output$plot4 <- renderPlot({
      data = as.data.frame(change_data())
      p = ggplot(data, aes(Day, prob_better,colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      return(p)
    })
    #-------------------------------------------------------------------------------
    output$B1 <- renderPlot({
      data = as.data.frame(sim_data())
      p = ggplot(data, aes(Day, Post_mean, colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      p <- p + geom_ribbon(aes(ymin=data$Cred_LL, ymax=data$Cred_UL), linetype=2, alpha=0.1)
      return(p)
    })
    output$F1 <- renderPlot({
      data = as.data.frame(sim_data())
      p = ggplot(data, aes(Day, CRate, colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      p <- p + geom_ribbon(aes(ymin=data$Conf_LL, ymax=data$Conf_UL), linetype=2, alpha=0.1)
      return(p)
    })
    output$B2 <- renderPlot({
      data = as.data.frame(change_data())
      p = ggplot(data, aes(Day, Post_mean ,colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      p <- p + geom_ribbon(aes(ymin=data$Cred_LL, ymax=data$Cred_UL), linetype=2, alpha=0.1)
      return(p)
    })
    output$F2 <- renderPlot({
      data = as.data.frame(change_data())
      p = ggplot(data, aes(Day, CRate_change, colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      p <- p + geom_ribbon(aes(ymin=data$Conf_LL, ymax=data$Conf_UL), linetype=2, alpha=0.1)
      return(p)
    })    
    #-------------------------------------------------------------------------------
    #--------- Group trend plots - Bayesian / Frequentist-------------------------
    #-------------------------------------------------------------------------------
    output$Bayesian_group_plot <- renderPlot({
      data = as.data.frame( sim_data() )
      Bgroup <- Freq_plot(CR = data , num_tests=3, Bayes = TRUE)
        return(Bgroup)
    })
    #-------------------------------------------------------------------------------
    output$Freq_group_plot <- renderPlot({
      data = as.data.frame( sim_data() )
      Fgroup <- Freq_plot(CR = data , num_tests=3, Bayes = FALSE) 
        return(Fgroup)
    })

    #-------------------------------------------------------------------------------
    # ------- Rate change plots - Bayesian / Frequentist-------------------------
    #-------------------------------------------------------------------------------
    output$Bayesian_change_plot <- renderPlot({
      data <- as.data.frame( change_data() )
      Bchange <- Change_plot(CR_change =  data , Bayes = TRUE)
      return( Bchange)
    })
    #-------------------------------------------------------------------------------
    output$Freq_change_plot <- renderPlot({
      data <- as.data.frame( change_data() )
      Fchange <- Change_plot(CR_change = data , Bayes = FALSE) 
      return(Fchange)
    })
    
    #-------------------------------------------------------------------------------
    # --------------------------A/B Test plots ---------------------------
    #-------------------------------------------------------------------------------
    output$BF_plot <- renderPlot({
      data <- as.data.frame( change_data() )
      BF <- plot_change_column(CR_change= change_data(), variable= "logBF", 
                               var_label= "log Bayes Factor", plot_max_pct = 0.5,
                               hline= as.numeric(input$Conf_alpha) ) 
      return( BF)
    })
    #-------------------------------------------------------------------------------
    output$pval_plot <- renderPlot({
      data <- as.data.frame( change_data() )
      pval <- plot_change_column(CR_change= data, variable= "p_value", 
                                    var_label= "p-value", 
                                    hline= as.numeric(input$Conf_alpha) ) 
      return(pval)
    })
    #-------------------------------------------------------------------------------
    output$Uplift_plot <- renderPlot({
      data <- as.data.frame( change_data() )
      Uplift <- plot_change_column(CR_change= data , 
                                   variable= "Uplift", var_label= "Uplift (%)") 
      return( Uplift)
    })
    #-------------------------------------------------------------------------------
    output$prob_better_plot <- renderPlot({
      data <- as.data.frame( change_data() )
      prob_better <- plot_change_column(CR_change= data , variable= "prob_better", 
                                        var_label= "Probability of better than default(%)" ) 
      return(prob_better)
    })
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    # download codes: 
    output$downloadUI <- downloadHandler(
    filename = "ui.R",
    myfile <- paste0('/srv/shiny-server/apps/files/',filename ),
    content = function(file) {   file.copy( myfile , file) }
    )
    output$downloadServer <- downloadHandler(
      filename = "server.R",
      myfile <- paste0('/srv/shiny-server/apps/files/', filename ),
      content = function(file) {   file.copy( myfile , file) }
    )
    output$downloadFunction <- downloadHandler(
      filename = "functions.R",
      myfile <- paste0('/srv/shiny-server/apps/files/',filename ),
      content = function(file) {   file.copy( myfile , file) }
    )
    
    output$downloadData <- downloadHandler(
      filename = function() { paste(input$dataset, '.csv', sep='') },
      content = function(file) {
        write.csv(datasetInput(), file)
      }
    )
    output$downloadAnalyze <- downloadHandler(
      filename = function() { paste(input$dataset, '.csv', sep='') },
      content = function(file) {
        write.csv(datasetInput(), file)
      }
    )    
    #-------------------------------------------------------------------------------
    upload_data <- reactive({
      inFile <- input$upload_raw_data
      
      # read default csv if not file input
      if (is.null(inFile)){
        tbl <- read.csv("n10K.csv") 
      }
      else{  tbl <- read.csv(inFile$datapath )}
      #----------------------
      # aggregate counts and compute group rate statistics 
      new_df <- transform_data( tbl,   # data frame 
                                a = as.numeric(input$Conf_alpha) ,  # confidence level
                                a_0 = input$alpha_0_upload, b_0= input$beta_0_upload) # Beta prior parameter
      return( new_df)
    })
    
    #-------------------------------------------------------------------------------
    # calculate change of conversion rate compared with default, point estimate, CI
    #-------------------------------------------------------------------------------
    upload_change <- reactive({
      CR_change = Cal_all_change(CR = upload_data() ,  
                                 Conf_alpha = as.numeric(input$Conf_alpha) ,
                                 alpha_0 = input$alpha_0_sim, beta_0= input$beta_0_sim ) 
      return(CR_change) 
    })
    
    #-------------------------------------------------------------------------------
    # renderTable to display 
    #-------------------------------------------------------------------------------
    
    output$upload_data <- renderTable({
      return(upload_data() )
    })
    
    output$upload_change <- renderTable({
      return(upload_change() )
    })
    #-------------------------------------------------------------------------------
    # Plots -BF, p-value, uplift, prob_better
    #-------------------------------------------------------------------------------
    output$plot1u <- renderPlot({
      data = as.data.frame(upload_change())
      p = ggplot(data, aes(Day, logBF,colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      return(p)
    })
    output$plot2u <- renderPlot({
      data = as.data.frame(upload_change())
      p = ggplot(data, aes(Day, p_value,colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      return(p)
    })
    output$plot3u <- renderPlot({
      data = as.data.frame(upload_change())
      p = ggplot(data, aes(Day, Uplift,colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      return(p)
    })
    output$plot4u <- renderPlot({
      data = as.data.frame(upload_change())
      p = ggplot(data, aes(Day, prob_better,colour= as.factor(Test_group) ))  +  
        geom_point() +  geom_line() + theme(legend.position="bottom")
      return(p)
    })
    #-------------------------------------------------------------------------------
    # Conversion rate change plot
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    #--------- Group trend plots - Bayesian / Frequentist-------------------------
    #-------------------------------------------------------------------------------
    output$B1u <- renderPlot({
      data = as.data.frame( sim_data() )
      Bgroup <- Freq_plot(CR = data , num_tests=3, Bayes = TRUE)
      return(Bgroup)
    })
    #-------------------------------------------------------------------------------
    output$F1u <- renderPlot({
      data = as.data.frame( sim_data() )
      Fgroup <- Freq_plot(CR = data , num_tests=3, Bayes = FALSE) 
      return(Fgroup)
    })
    
    #-------------------------------------------------------------------------------
    # ------- Rate change plots - Bayesian / Frequentist-------------------------
    #-------------------------------------------------------------------------------
    output$B2u <- renderPlot({
      data <- as.data.frame( change_data() )
      Bchange <- Change_plot(CR_change =  data , Bayes = TRUE)
      return( Bchange)
    })
    #-------------------------------------------------------------------------------
    output$F2u <- renderPlot({
      data <- as.data.frame( change_data() )
      Fchange <- Change_plot(CR_change = data , Bayes = FALSE) 
      return(Fchange)
    }) 
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    
    formulaText <- reactive({
        paste("Analysis at Confidence Level 1-", input$Conf_alpha)
    })
    output$caption <- renderText({
        formulaText()
    })
    
    #-------------------------------------------------------------------------------


})
