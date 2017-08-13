#  A web app to perform Bayesian A/B Testing
#  Qiaolin Chen
#-------------------------------------------------------------------------

library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Bayesian A/B Test", tabName="plot", icon=icon("line-chart"), selected=TRUE),
              menuItem("A/B Test on simulated data", tabName = "simulate", icon=icon("table")),
              menuItem("Upload and Analyze Data", tabName = "table", icon=icon("dashboard")),
              menuItem("Jupyter Notebook", tabName = "readme", icon=icon("mortar-board")),
              menuItem("Codes",  icon = icon("file-text-o"),
                       menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                       menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
                       menuSubItem("functions.R", tabName = "functions", icon = icon("angle-right"))
              )

  ),
  hr(),
  conditionalPanel(condition = "input.tabs in c('plot','simulate','table') " ,
                   fluidRow(
                     column(1),
                     column(10,

                    selectInput(inputId ="Conf_alpha", label = "Confidence Level",
                        choices =list("90%" = 0.1, "95%" = 0.05 ,  "99%" = 0.01 ),
                        selected = 0.1 ) 
                     )
                   )
   )
)
#-----------------------------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "readme",
            withMathJax(), 
            includeMarkdown("Juypter_notebook/Bayesian_AB_testing.md")
            #includeHTML("Bayesian_AB_testing.html")
 
    ),

 #------------------------------------------------------------------------------------

    tabItem(tabName = "plot", 
            box( width = NULL,  solidHeader = TRUE,  collapsible = TRUE, status = "primary",
                 title="Input Test Data and Prior Parameters",
                          fluidRow( column(3, 
                                       numericInput(inputId = "nA",min = 0, step=1, 
                                                    label = "Users A:", value = 110 ),                                       
                                       numericInput(inputId = "nB", min = 0, step=1, 
                                                    label = "Users B:", value = 90)                                   
                                       ),
                                  column(3, 
                                       numericInput(inputId = "xA", min = 0, step=1, 
                                                    label = "Conversions A:", value = 20),
                                       numericInput(inputId = "xB", min = 0, step=1, 
                                                    label = "Conversions B:", value = 29)
                                       ) ,
                                column(3, 
                                    numericInput(inputId = "alpha_0",  min = 0, step=0.1, 
                                                 label = "alpha:", value = 1),
                                    numericInput(inputId = "beta_0",  min = 0, step=0.1, 
                                                 label = "beta:", value = 1)
                                    )
                          ) 
                  #, submitButton("Perform A/B Test")
                     ),
                     box(  width = NULL, tableOutput("single_ABtest"),
                        collapsible = TRUE,
                           title = "Bayesian A/B Test Results", status = "primary", solidHeader = TRUE),
                     box(  width = NULL,
                           plotOutput("ABtest_density",height="400px"), 
                           fluidRow( 
                             column(6 , plotOutput("ABtest_change_density",height="350px") ),
                             column(6, plotOutput("ABtest_bestProb",height="300px")  )

                                    ),
                           collapsible = TRUE,
                           title = "Prior and Posterior density", status = "primary", solidHeader = TRUE)
                    

              
    ),
    #------------------------------------------------------------------------------------
 tabItem(tabName = "simulate", #headerPanel("A/B Test Analysis"),
         fluidRow(
           column(width = 3, 
                  box( width = NULL,  solidHeader = TRUE, 
                       title="Set True Values for Simulation", 
                                   #submitButton("Simulate Data"),
                                   br(), 
                                   sliderInput(  inputId = "pA",
                                                 label = "Test 0 Convertion Rate (%):", value = 2,  
                                                 min = 0, max = 100, step= 0.5),
                                   sliderInput(  inputId = "pB",
                                                 label = "Test 1 Convertion Rate (%):", value = 4,  
                                                 min = 0, max = 100, step= 0.5),
                                   sliderInput(  inputId = "pC",
                                                 label = "Test 2 Convertion Rate (%):", value = 2.3,  
                                                 min = 0, max = 100, step= 0.5),
                                   #sliderInput(  inputId = "pD",
                                   #               label = "Test D Convertion Rate (%):", value = 0,  
                                   #               min = 0, max = 100, step= 1),
                                   sliderInput(  "counts",  
                                                label = "Total users for each test:", 
                                               min = 500, max = 20000, value = 5000, step= 1),
                                   sliderInput(  "test_duration",  
                                                 label = "Duration of tests:", 
                                                 min = 10, max = 100, value = 60, step= 1),
     
                                h5("Beta Prior Parameters"),
                                fluidRow(
                                    column(width = 5,
                                           numericInput(inputId = "alpha_0_sim", label = "alpha:",
                                                        value = 1.0, min = 0, step=0.1 )
                                           ),
                                    column(width = 5,                                     
                                          numericInput(inputId = "beta_0_sim", label = "beta:", 
                                                       value = 1.0, min = 0, step=0.1  )
                                          )
                                ),
                       dateInput('start_date',
                                 label = 'Start Date input: yyyy-mm-dd',
                                 value = Sys.Date()
                       )#,
                       
                       #br(),br(),                                  
                       #downloadButton('downloadTable', 'Download raw data'),
                       #br(),br(),
                       #tableOutput("table1"),
                       #downloadButton('downloadTable1', 'Download analyzed data'),
                       #br(),br(),
                       #tableOutput("table2")
                          
                  )),
           #------------------------------------------------------------------------------
           column(width = 9 ,
                  box(  width = NULL, tableOutput('sim_data'),
                        collapsible = TRUE, collapsed = TRUE,
                        title = 'Data Simulated', status = "primary", solidHeader = TRUE),
                    #--------------------------------------------------------------------
                  box(  width = NULL, tableOutput('Conv_change'),
                        collapsible = TRUE, collapsed = TRUE,
                        title = 'Data Computed - Conversion Rate Change', status = "primary", solidHeader = TRUE),  

                  #box(  width = NULL, plotOutput('hist')  ,
                  #      collapsible = TRUE,  
                  #      title = 'Histogram', status = "primary", solidHeader = TRUE),  
                  box(  width = NULL, collapsible = TRUE,status = "primary",
                        title = "Conversion Rate Change Over time",  solidHeader = TRUE,
                        tabsetPanel( 
                          #tabPanel("Bayesian-Change", plotOutput("B2", height="400px") ) ,
                          #tabPanel("Frequentist-Change", plotOutput("F2", height="400px") ),
                          #tabPanel("Bayesian-Rate",plotOutput("B1", height="400px") ),
                          #tabPanel("Frequentist-Rate", plotOutput("F1", height="400px") )
                          
                          tabPanel("Bayesian-Change", plotOutput("Bayesian_change_plot", height="400px") ) ,
                          tabPanel("Frequentist-Change", plotOutput("Freq_change_plot", height="400px") ),
                          tabPanel("Bayesian-Rate",plotOutput("Bayesian_group_plot", height="400px") ),
                          tabPanel("Frequentist-Rate", plotOutput("Freq_group_plot", height="400px") )
                        )
                  ),
                  
                  box(  width = NULL, collapsible = TRUE,status = "primary",
                        title = "A/B Test Plots",  solidHeader = TRUE,
                        tabsetPanel( 
                          tabPanel("Bayes Factor", plotOutput("plot1", height="400px") ) ,
                          tabPanel("p-value", plotOutput("plot2", height="400px") ),
                          tabPanel("Uplift",plotOutput("plot3", height="400px") ),
                          tabPanel("Prob of better than default", plotOutput("plot4", height="400px") )
                          
                          #tabPanel("Bayes Factor", plotOutput("BF_plot", height="400px") ) ,
                          #tabPanel("p-value", plotOutput("pval_plot", height="400px") ),
                          #tabPanel("Uplift",plotOutput("Uplift_plot", height="400px") ),
                          #tabPanel("Prob of better than default", plotOutput("prob_better_plot", height="400px") )
                        )
                  )
                  )
           
         )
 ),
 
    tabItem(tabName = "table",
            box( width = NULL, status = "primary", solidHeader = TRUE, 
                 title="Upload and analyze A/B test data",

                # Sidebar with controls to provide a caption, select a dataset,
                # and specify the number of observations to view. Note that
                # changes made to the caption in the textInput control are
                # updated in the output area immediately as you type
                #h3(textOutput("caption", container = span)),
                #        verbatimTextOutput("summary"),

                # make a file upload manager
                h5("Upload a CSV file with 3 columns: Test_group (0,1,...), Date (YYYY-MM-DD), and Convert (1=yes, 0=No)."),
                h5("If no data upload, an example data set will be plot and analyzed."),
                fileInput("upload_raw_data", "Choose CSV File",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
                ),
                fluidRow( column(3, h5("Input parameters for Beta prior: ")                                  
                ),
                column(2, 
                       numericInput(inputId = "alpha_0_upload",  min = 0, step=0.1, 
                                    label = "alpha:", value = 1)
                ) ,
                column(2, 
                       numericInput(inputId = "beta_0_upload",  min = 0, step=0.1, 
                                    label = "beta:", value = 1)
                 )
                ),  
                
                #submitButton("Upload and Perform A/B Test"), 
                hr(),
                box(  width = NULL, tableOutput('upload_data'),
                      collapsible = TRUE, collapsed = TRUE,
                      title = 'Data Uploaded and Converted', status = "primary", solidHeader = TRUE),
                #--------------------------------------------------------------------
                box(  width = NULL, tableOutput('upload_change'),
                      collapsible = TRUE, collapsed = TRUE,
                      title = 'Conversion Rate Change', status = "primary", solidHeader = TRUE),  
                
                #box(  width = NULL, plotOutput('hist')  ,
                #      collapsible = TRUE,  
                #      title = 'Histogram', status = "primary", solidHeader = TRUE),  
                box(  width = NULL, collapsible = TRUE,status = "primary",
                      #collapsed = TRUE,
                      title = "Conversion Rate Change Over time",  solidHeader = TRUE,
                      tabsetPanel( 
                        tabPanel("Bayesian-Change", plotOutput("B2u", height="400px") ) ,
                        tabPanel("Frequentist-Change", plotOutput("F2u", height="400px") ),
                        tabPanel("Bayesian-Rate",plotOutput("B1u", height="400px") ),
                        tabPanel("Frequentist-Rate", plotOutput("F1u", height="400px") )
                      )
                ),
                
                box(  width = NULL, collapsible = TRUE,status = "primary",
                      #collapsed = TRUE,
                      title = "A/B Test Plots",  solidHeader = TRUE,
                      tabsetPanel( 
                        tabPanel("Bayes Factor", plotOutput("plot1u", height="400px") ) ,
                        tabPanel("p-value", plotOutput("plot2u", height="400px") ),
                        tabPanel("Uplift",plotOutput("plot3u", height="400px") ),
                        tabPanel("Prob of better than default", plotOutput("plot4u", height="400px") )
                        
                      )
                ) 

            )
    ),
    #------------------------------------------------------------------------------------

    tabItem(tabName = "functions",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="functions.R",
                 #downloadButton('downloadFunction', 'Download'),
                 br(),br(),
                 pre(includeText("functions.R"))
            )
    ),
    tabItem(tabName = "ui",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
                 #downloadButton('downloadUi', 'Download'),
                 br(),br(),
                 pre(includeText("ui.R"))
            )
    ),
    tabItem(tabName = "server",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
                 #downloadButton('downloadServer', 'Download'),
                 br(),br(),
                 pre(includeText("server.R"))
            )
    )
  )
)
#------------------------------------------------------------------------------------

dashboardPage(
  dashboardHeader(title = "Bayesian A/B Test"),
  sidebar,
  body
)
