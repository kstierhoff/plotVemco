#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(ggplot2);library(mapproj)

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
    
    # User-defined input file
    fileInput('stationData',"Select hydrophone data file:",
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
                min=5, max=60, value=15),
    
    # Maximum Horizontal Position Error (HPE)
    sliderInput("minTriangles", "Min. triangles used for position:",
                min=0, max=7, value=1) 
    # # Date range input selection
    # ,dateRangeInput("daterange", "Date range:",
    #                start = Sys.Date()-10,
    #                end = Sys.Date()+10)
  ),
  
  mainPanel(
    #  Application title
    titlePanel("Interactive Vemco tag data explorer"),
    # First level text
    h3("Overview"),
    # Descriptive text
    p("This in an interactive web app for exploring tag detections from a Vemco hydrophone array. 
      Load a standard Vemco 'position' file from your local machine, and use the various sliders to 
      filter the data based on horizontal position error (HPE), number of receiver triangles used
      to estimate the position, etc."),
    # Plot the tag data map
    h4("Tag map"),
    p("A map of all tag detections (gray points) and tag detections after filters have been applied (colored points)."),
    plotOutput('map'),
    # Plot HPE histrograms for each tag
    h4("Horizontal position error (HPE) histrograms"),
    p("Frequency histograms of tag detections after filters have been applied."),
    plotOutput('histHPE')
  )
)

# Define server output
server <- function(input, output) {
  # set limit for file size
  options(shiny.maxRequestSize=30*1024^2)
  # # read station data
  # stations <- read.csv("../../Data/stations.csv")
  # Read user-defined data file
  tag.data <- reactive({
    inFile <- input$tagData
    if (is.null(inFile)) return(NULL)
    
    read.csv(inFile$datapath,header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
  # Read user-defined station data file
  station.data <- reactive({
    inFile <- input$stationData
    if (is.null(inFile)) return(NULL)
    
    read.csv(inFile$datapath,header=input$header, sep=input$sep, 
             quote=input$quote)
  })

  # Map of tag detections for all animals, filtered by slider settings
  output$map <- renderPlot({
    tagIn <- input$tagData
    
    if (is.null(tagIn))  return(NULL)
    df = tag.data()

    ggplot() +
      geom_point(data = df,aes(LON,LAT),colour = 'gray50',alpha = 0.5) +
      geom_point(data = subset(df,HPE <= input$maxHPE & n >= input$minTriangles),aes(LON,LAT,colour = DETECTEDID)) +
      xlab("\nLongitude (W)") + ylab("Latitude (N)\n") + 
      theme_bw() + theme(plot.background=element_blank(),
                         axis.text.y = element_text(angle = 90, hjust=0.5),
                         panel.margin=unit(1, "lines")) +
      coord_map()
  })
  
  # Histogram of tag HPEs, filtered by slider settings
  output$histHPE <- renderPlot({
    inFile <- input$tagData
    
    if (is.null(inFile))  return(NULL)
    df = tag.data()
    
    ggplot() + 
      # geom_histogram(data = df,aes(HPE),colour = 'gray50') + 
      geom_histogram(data = subset(df,HPE <= input$maxHPE & n >= input$minTriangles),aes(HPE),colour = 'black') + 
      facet_wrap(~DETECTEDID,scales = "free_y") +
      xlab("\nHorizontal Position Error") + ylab("Frequency\n") +
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

