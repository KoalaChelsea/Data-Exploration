library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

dashboardPage(skin="blue",
              
              #Title
              dashboardHeader(title="Data Exploration",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(
                  menuItem("Overview", tabName = "over", icon = icon("university")),
                  menuItem("Sample Data", tabName = "sample", icon = icon("pencil-square")),
                  menuItem("Data Upload", tabName = "upload", icon = icon("pencil-square"))
                  #menuItem("Paste Dataset", tabName = "create", icon = icon("pencil-square"))
                )),
              
              #Content within the tabs
              dashboardBody(tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
              ),
                
                
                tabItems(
                  
                  tabItem(tabName = "over",
                          
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(12,
                                   h3(strong("About")),
                                   h4("This tool provides data summaries and plots for sample data sets and for data that can be uploaded by by users."),
                                   h4(strong("Instructions:")),
                                   h4("In sample data, you can play with some existing datasets. Pick two variables to view data displays, summaries and plots based on your variable selection."),
                                   h4("In data upload, you can upload your own dataset in csv, txt or Excel formats. The setting are very similar with sample data page."),
                                   h4(strong("Acknowledgements:")),
                                   h4("This app was developed and coded by Yingjie(Chelsea) Wang. Homepage on github https://github.com/KoalaChelsea"),
                                   h4("Sample data Flash is from data set library in Minitab. More information about this datasets can be found at https://support.minitab.com/en-us/datasets/graphs-data-sets/flash-recovery-time-data/."),
                                   h4("Sample data Diamonds is from Package Stat2Data. More information about this datasets can be found at transcription 22 at the website http://docplayer.net/26320271-Package-stat2data-r-topics-documented-february-19-2015.html."),
                                   h4(""),
                                   #h4("In paste data, you can paste some columns with numeric variable from data file into the box."),
                                   h4("Have fun with it.")
                            )
                          )
                  ),
                  tabItem(tabName = "sample",
                          fluidPage(
                            titlePanel("Play with sample data"),
                            sidebarLayout(
                              sidebarPanel(
                                h4(strong("Instructions:")),
                                h4("Pick two variables to view data displays, summaries and plots based on your variable selection."),
                                # selectInput("sampleData", "Choose a sample dataset:", 
                                #             choices = c("dismonds", "flash")),
                                radioButtons("sampleData", "Load sample data", list("Diamonds"=1,"FlashLife"=2)),
                                uiOutput("samplevariable1"),
                                uiOutput("samplevariable2"),
                                br()
                                # downloadButton('downloadData', 'Download Data'),
                                # downloadButton("downloadPlotPNG", "Download png-file")
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Data Display",
                                                     dataTableOutput('sampledisplay')
                                            ),
                                            
                                            tabPanel("Data Summary",
                                                     verbatimTextOutput('samplesummary'),
                                                     br(),
                                                     verbatimTextOutput('samplestr')
                                            ),
                                            tabPanel("Data plot",
                                                     plotOutput("sampleplot1"),
                                                     plotOutput("sampleplot2"),
                                                     plotOutput("sampleplot3")
                                            )
                                )
                              )
                            )
                          )
                  ),
                 
                    tabItem(tabName = "upload",
                            fluidPage(
                              titlePanel("Uploading Files"),
                              sidebarLayout(
                                sidebarPanel(
                                  h4(strong("Instructions:")),
                                  h4("Pick two variables to view data displays, summaries and plots based on your variable selection."),
                                  uiOutput("fileInclude"),
                                  fileInput('file1', 'Choose Data File:',
                                            accept=c('.csv', '.txt', '.xls',
                                                     '.xlsx', '.sas7bdat')),
                                  checkboxInput('header', 'Header', TRUE),
                                  br(),
                                  uiOutput("variable1"),
                                  uiOutput("variable2"),
                                  br(),
                                  h5("Note: The column name in the data file should be a single word. Or the data file can't be uploaded successfully.")
                                  
                                  
                                  # Only show this panel if the plot type is a histogram
                                  #uiOutput("varSelect")
                                  
                                ),
                                mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Data Display",
                                                       dataTableOutput('display')
                                              ),
                                              
                                              tabPanel("Data Summary",
                                                       verbatimTextOutput('summary'),
                                                       br(),
                                                       verbatimTextOutput('str')
                                              ),
                                              tabPanel("Data plot",
                                                       plotOutput("plot1"),
                                                       plotOutput("plot2"),
                                                       plotOutput("plot3")
                                                       # plotOutput("numplots"),
                                                       # plotOutput("factorplots"),
                                                       # plotOutput("mixplots")
                                              )
                                  )
                                )
                              )
                            )
                    ),
                  tabItem(tabName = "create",
                          fluidPage(
                            titlePanel("Paste data here"),
                            sidebarLayout(
                              sidebarPanel(
                                h3("Paste some columns in data file below:"),
                                tags$textarea(id="myData", rows=15, cols=15, ""),
                                h4("Select the right seperator:"),
                                radioButtons("mysep", "Separator:", list("Comma"=1,"Tab"=2,"Semicolon"=3)),
                                br(),
                                uiOutput("pastevariable1"),
                                
                                h5("Note: Please ignore the error message before you paste your data in."),
                                h5("Note: The column name in the data file should be a single word. Or the data file can't be shown successfully.")
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Data Display",
                                                     tableOutput("filetable")
                                            ),
                                            
                                            tabPanel("Data Summary",
                                                     verbatimTextOutput('pastesummary'),
                                                     br(),
                                                     verbatimTextOutput('pastestr')
                                            ),
                                            tabPanel("Data plot",
                                                     plotOutput("pasteplot1")
                                                    
                                            )
                                )
                              )
                            )
                          )
                  )
                )#end of tabItems
              )#end of dashboardBody
)










