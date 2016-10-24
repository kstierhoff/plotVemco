#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(ggplot2)

# Define UI for application
ui <- fluidPage(
  sidebarPanel(
    # User-defined input file
    fileInput('tagData',"Select data file:",
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',', Semicolon=';', Tab='\t'),','),
    radioButtons('quote', 'Quote',
                 c(None='','Double Quote'='"','Single Quote'="'"),'"'),
    # Maximum Horizontal Position Error (HPE)
    sliderInput("maxHPE", "Max Horizontal Position Error:",
                min=0, max=300, value=300)
  ),
  
  mainPanel(
    # Display head of input file
    # tableOutput('preview'),
    plotOutput('map'),
    plotOutput('histHPE')
    
  )
   
   
)

# Define server output
server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  # Read user-defined data file
  filedata <- reactive({
    inFile <- input$tagData
    if (is.null(inFile)) return(NULL)
    
    read.csv(inFile$datapath,header=input$header, sep=input$sep, 
             quote=input$quote)
  }) 
  
  output$preview <- renderTable({
     inFile <- input$tagData
     
     if (is.null(inFile))  return(NULL)
     df = filedata()
     head(df)
   })
  
  # map tag detections
  output$map <- renderPlot({
    inFile <- input$tagData
    
    if (is.null(inFile))  return(NULL)
    df = filedata()
    
    ggplot(data = subset(df,HPE <= input$maxHPE),aes(LON,LAT,group = DETECTEDID)) + 
      geom_point(aes(colour = HPE)) + 
      scale_colour_gradientn(colours=rev(rainbow(7))) +
      xlab("\nLongitude (W)") + ylab("Latitude (N)\n") + 
      theme_bw() + theme(plot.background=element_blank(),
                         axis.text.y = element_text(angle = 90, hjust=0.5),
                         panel.margin=unit(1, "lines")) +
      coord_map()
  })
  
  # histogram of tag HPEs
  output$histHPE <- renderPlot({
    inFile <- input$tagData
    
    if (is.null(inFile))  return(NULL)
    df = filedata()
    
    ggplot(data = subset(df,HPE <= input$maxHPE),aes(HPE)) + 
      geom_histogram() + 
      xlab("\nHorizontal Position Error") + ylab("Frequency\n") +
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

