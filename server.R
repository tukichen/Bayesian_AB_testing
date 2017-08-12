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

    output$plot <- renderPlot({
        
        hist(faithful$eruptions,
        probability = TRUE,
        breaks = 20,
        xlab = "Duration (minutes)",
        main = "Geyser eruption duration")
        
        if (input$individual_obs) {
            rug(faithful$eruptions)
        }
    })
   #-------------------------------------------------------------------------------

    
    output$ABtest <- renderTable({
      result <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                                alpha_0 = input$alpha_0, beta_0= input$beta_0  )
        return(result)
    })
    #-------------------------------------------------------------------------------
    
    output$ABtest_density <- renderPlot({
      density_plot <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                               alpha_0 = input$alpha_0, beta_0= input$beta_0, 
                               density_plot =TRUE )
      return(density_plot)
    })
    
    output$ABtest_bestProb <- renderPlot({
      bestProb_plot <- Bayes_AB_test( nA = input$nA, xA= input$xA, nB = input$nB, xB = input$xB, 
                                     alpha_0 = input$alpha_0, beta_0= input$beta_0, 
                                     bestProb_plot =TRUE )
      return(bestProb)
    })   
    #-------------------------------------------------------------------------------
    
    output$table <- renderTable({ 
        rr()
    })
    #-------------------------------------------------------------------------------
    
    
    output$downloadTable <- downloadHandler(
    filename = "table.csv",
    content = function(file) {
        write.csv(rr(), file)
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
