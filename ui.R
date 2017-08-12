#-------------------------------------------------------------------------
#  A web app to perform Bayesian A/B Testing
#  Qiaolin Chen
#-------------------------------------------------------------------------

library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Bayesian A/B Test", tabName="plot", icon=icon("line-chart"), selected=TRUE),
              menuItem("A/B Test on simulated data", tabName = "simulate", icon=icon("table")),
              menuItem("Upload and Analyze Data", tabName = "table", icon=icon("table")),
              menuItem("Codes",  icon = icon("file-text-o"),
                       menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                       menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
                       menuSubItem("functions.R", tabName = "functions", icon = icon("angle-right"))
              ),
              menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board"))
  ),
  hr(),
  conditionalPanel("input.tabs=='plot'",
                   fluidRow(
                     column(1),
                     column(10,

                    selectInput(inputId ="alpha", label = "Confidence Level",
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
            includeMarkdown("README.md")
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
                          ),
                          submitButton("Perform A/B Test")
                     ),
                     box(  width = NULL, tableOutput("single_ABtest"),
                        collapsible = TRUE,
                           title = "Bayesian A/B Test Results", status = "primary", solidHeader = TRUE),
                     box(  width = NULL,
                           plotOutput("ABtest_density",height="450px"), 
                           fluidRow( 
                             column(6 , plotOutput("ABtest_change_density",height="400px") ),
                             column(6, plotOutput("ABtest_bestProb",height="300px")  )

                                    ),
                           collapsible = TRUE,
                           title = "Prior and Posterior density", status = "primary", solidHeader = TRUE)
                    

              
    ),
    #------------------------------------------------------------------------------------
 tabItem(tabName = "simulate", headerPanel("A/B Test Analysis"),
         fluidRow(
           column(width = 4, 
                  tabBox( width = NULL,
                          tabPanel(h5("Set True Values"),
                                   sliderInput(  inputId = "pA",
                                                 label = "Test A Convertion Rate (%):", value = 2,  
                                                 min = 0, max = 100, step= 1),
                                   sliderInput(  inputId = "pB",
                                                 label = "Test B Convertion Rate (%):", value = 4,  
                                                 min = 0, max = 100, step= 1),
                                   sliderInput(  inputId = "pC",
                                                 label = "Test C Convertion Rate (%):", value = 2.5,  
                                                 min = 0, max = 100, step= 1),
                                   #sliderInput(  inputId = "pD",
                                   #               label = "Test D Convertion Rate (%):", value = 0,  
                                   #               min = 0, max = 100, step= 1),
                                   sliderInput(  "counts",  
                                                label = "Total users for each test:", 
                                               min = 500, max = 20000, value = 10000, step= 1),
                                   sliderInput(  "test_duration",  
                                                 label = "Duration of tests:", 
                                                 min = 10, max = 300, value = 60, step= 1),
                                   dateInput('start_date',
                                             label = 'Start Date input: yyyy-mm-dd',
                                             value = Sys.Date()
                                   ),
                                   submitButton("Simulate Data and Perform Tests"),
                                   
                                   br(),br(),                                  
                                   downloadButton('downloadTable', 'Download simulated data'),
                                   br(),br(),
                                   tableOutput("table")
                          ),
                          tabPanel(h5("Beta Prior Parameters"),
                                   numericInput(inputId = "alpha_0_sim",
                                                label = "alpha:",
                                                value = 1),
                                   numericInput(inputId = "beta_0_sim",
                                                label = "beta:",
                                                value = 1),
                                   submitButton("Simulate Data and Perform Tests")

                          )
                  )),
           column(width = 8 #,
                  
                  #box(  width = NULL, tableOutput("single_ABtest"),
                  #      collapsible = TRUE,
                  #      title = "Bayesian A/B Test Results", status = "primary", solidHeader = TRUE),
                  #box(  width = NULL,
                  #      plotOutput("ABtest_density",height="450px"), 
                  #      plotOutput("ABtest_bestProb",height="250px"), collapsible = TRUE,
                  #      title = "Prior and Posterior density", status = "primary", solidHeader = TRUE)
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
                    sidebarLayout(
                        sidebarPanel(
                            textInput("caption", "Caption:", "Data Summary"),

                        selectInput("dataset", "Choose a dataset:",
                                choices = c("n10K", "n20K","n5K", "n10K_closer")),
                        numericInput("obs", "Number of observations to view:", 10)
                            ),

                    # Show the caption, a summary of the dataset and an HTML
                    # table with the requested number of observations
                    mainPanel(
                    h3(textOutput("caption", container = span)),

                        verbatimTextOutput("summary"),

                            tableOutput("view")
                        )
                ),

                # make a file upload manager
                fileInput("file", label = h5("Upload a CSV file with 3 columns: Test_group (0,1,...), Date (YYYY-MM-DD), and Convert (1=yes, 0=No)")),

                hr(),
                fluidRow(column(5, verbatimTextOutput("value")))


            )
    ),
    #------------------------------------------------------------------------------------

    tabItem(tabName = "functions",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="functions.R",
                 downloadButton('downloadData1', 'Download'),
                 br(),br(),
                 pre(includeText("functions.R"))
            )
    ),
    tabItem(tabName = "ui",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
                 downloadButton('downloadData2', 'Download'),
                 br(),br(),
                 pre(includeText("ui.R"))
            )
    ),
    tabItem(tabName = "server",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
                 downloadButton('downloadData3', 'Download'),
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
