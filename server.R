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

shinyServer(function(input, output){

    output$plot <- renderPlot({
        
        hist(faithful$eruptions,
        probability = TRUE,
        breaks = as.numeric(input$n_breaks),
        xlab = "Duration (minutes)",
        main = "Geyser eruption duration")
        
        if (input$individual_obs) {
            rug(faithful$eruptions)
        }
        
        
    })

    
    rr <- reactive({
        r <- r()
        rr <- r[[1]]
        for (k in (2:6))
        rr <- merge(rr, r[[k]])
        return(rr)
    })
    
    output$table <- renderTable({ 
        rr()
    })
    
    output$downloadTable <- downloadHandler(
    filename = "table.csv",
    content = function(file) {
        write.csv(rr(), file)
    }
    )
    
})
