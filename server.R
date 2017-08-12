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
  # single AB_test resulst table  
  #-------------------------------------------------------------------------------
    
    output$single_ABtest <- renderTable({
      result <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                                alpha_0 = input$alpha_0, beta_0= input$beta_0  )
        return(result)
    })
    
    #-------------------------------------------------------------------------------
    # A/B Test density plot, posterior plot of change, plot of probability being better; 
    #-------------------------------------------------------------------------------
    output$ABtest_density <- renderPlot({
      density_plot <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                               alpha_0 = input$alpha_0, beta_0= input$beta_0, out_data = FALSE, 
                               density_plot =TRUE )
      return(density_plot)
    })
    
    output$ABtest_change_density <- renderPlot({
      density_plot <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                                     alpha_0 = input$alpha_0, beta_0= input$beta_0, out_data = FALSE, 
                                     diff_plot =TRUE )
      return(density_plot)
    })    
    
    output$ABtest_bestProb <- renderPlot({
      bestProb_plot <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                                     alpha_0 = input$alpha_0, beta_0= input$beta_0, out_data = FALSE, 
                                     bestProb_plot =TRUE )
      return(bestProb_plot)
    })   
    
    #-------------------------------------------------------------------------------
    # calculate point estimate and CI for conversion rate by group over time 
    #-------------------------------------------------------------------------------
    
    output$sim_data <- renderTable({
      num_test = 0
      prob_list = c() 
      Test_name = c()  
      if (input$pA > 0){ num_test = num_test +1 ; 
                        prob_list = c( prob_list, input$pA ) ; Test_name = c(Test_name, "A") }
      if (input$pB > 0){ num_test = num_test +1 ; 
                        prob_list = c( prob_list, input$pB ); Test_name = c(Test_name, "B") }
      if (input$pC > 0){ num_test = num_test +1 ; 
                        prob_list = c( prob_list, input$pC ); Test_name = c(Test_name, "C") }
      #if (input$pD > 0){ num_test = num_test +1 ; 
      #                  prob_list = c( prob_list, input$pD ; Test_name = c(Test_name, "D") )}
      
      df = simulate_data(num_tests , input$start_date, input$test_duration,
                         input$counts , prob_list, input$alpha, input$alpha_0_sim, input$beta_0_sim)
      new_df <- transform_data( df,   # data frame 
                     a = alpha ,  # confidence level
                     a_0 = input$alpha_0, b_0= input$beta_0) # Beta prior parameter
      return(new_df )
    })
    #-------------------------------------------------------------------------------
    
    # calculate change of conversion rate compared with default, point estimate, CI
    output$Conv_change <- renderTable({
      
      return( Cal_all_change(output$sim_data) )
    })
    
    #-------------------------------------------------------------------------------
    #output$Bayesian_change_plot <- renderTable({
    #    Bayes_plot <- 
    #})
    #-------------------------------------------------------------------------------
    output$Freq_change_plot <- renderTable({
      
    })
    #-------------------------------------------------------------------------------

    #-------------------------------------------------------------------------------
    
    
    output$downloadTable <- downloadHandler(
    filename = "table.csv",
    content = function(file) {
        write.csv(output$sim_data, file)
    }
    )
    
    formulaText <- reactive({
        paste("Analysis at Confidence", input$variable, "%")
    })
    output$caption <- renderText({
        formulaText()
    })
    
    #-------------------------------------------------------------------------------


})
