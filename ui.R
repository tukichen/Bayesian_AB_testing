#-------------------------------------------------------------------------
#  A web app to perform Bayesian A/B Testing
#  Qiaolin Chen
#-------------------------------------------------------------------------

library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Bayesian A/B Test", tabName="plot", icon=icon("line-chart"), selected=TRUE),
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
                    selectInput(inputId = "n_breaks",
                        label = "Number of bins in histogram (approximate):",
                        choices = c(10, 20, 35, 50),
                        selected = 20),

                    checkboxInput(inputId = "individual_obs",
                        label = strong("Show individual observations"),
                        value = FALSE)
                     )



                   )
  )

)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "readme",
            withMathJax(), 
            includeMarkdown("README.md")
    ),
    tabItem(tabName = "plot",
            fluidRow(
              column(width = 4, 
                     tabBox( width = NULL,
                             tabPanel(h5("Test Data"),
                                numericInput(inputId = "nA",
                                    label = "Users A:",
                                    value = 100),
                                numericInput(inputId = "nB",
                                    label = "Users B:",
                                    value = 100),
                                numericInput(inputId = "xA",
                                    label = "Conversions A:",
                                    value = 1),
                                numericInput(inputId = "xB",
                                    label = "Conversions B:",
                                    value = 2)



                             ),
                             tabPanel(h5("Beta Prior Parameters"),
                                    numericInput(inputId = "alphaA",
                                        label = "alpha:",
                                        value = 1),
                                    numericInput(inputId = "betaB",
                                        label = "beta B:",
                                        value = 1)

                             )
                     )),
              column(width = 8,
                     box(  width = NULL, plotOutput("plot",height="500px"), collapsible = TRUE,
                           title = "Plot", status = "primary", solidHeader = TRUE)
              ))
    ),
    tabItem(tabName = "table",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Upload and analyze A/B test data",

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
                fluidRow(column(5, verbatimTextOutput("value"))),

                 downloadButton('downloadTable', 'Download'),
                 br(),br(),
                 tableOutput("table")
            )
    ),
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

dashboardPage(
  dashboardHeader(title = "Bayesian A/B Test"),
  sidebar,
  body
)
