#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Vemco Tag Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # file input
        fileInput('tagData', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')
      ),
      mainPanel(
        tableOutput('contents'),
        # textOutput('')
        plotOutput('tagHist')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filedata <- reactive({
    infile <- input$tagData
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  output$tagHist <- renderPlot({
    if (is.null(df)) return(NULL)
    df <- filedata
    hist(df$HPE)
  })
  
  output$contents <- renderTable({
    
    # input$tagData will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$tagData
    
    if (is.null(inFile))
      return(NULL)
    
    head(read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote))
  })
  
  # output$hist <- renderPlot({
  #   hist.data <- input$tagData
  #   hist(HPE)
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

