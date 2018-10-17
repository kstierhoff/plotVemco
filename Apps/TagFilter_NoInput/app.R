#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(ggplot2);library(mapproj);library(dplyr);

options(shiny.maxRequestSize = 30*1024^2)

# LOAD DATA FILES #####
# Read tag data file
tag.data <- read.csv("../../Data/Examples/tag_example_big.csv")

# Read user-defined station data file
station.data <- read.csv("../../Data/Examples/station_example.csv")

# Define UI for application #####
ui <- fluidPage(
  sidebarPanel(
    fluidRow(
      # Maximum Horizontal Position Error (HPE)
      column(6,sliderInput("maxHPE", "Max Horizontal Position Error:",
                  min=0, max=max(tag.data$HPE), value=c(5,30))),
      
      # Maximum Horizontal Position Error (HPE)
      column(6,sliderInput("minTriangles", "Min. triangles used for position:",
                  min=0, max=max(tag.data$n), value=1))),
    fluidRow(
      h4("Select one or more tag IDs:"),
      p("Select a tag ID from the drop-down list. To remove a selection, highlight in the window and press 'Delete'"),
      # Select Tag ID (DETECTEDID)
      column(12,selectInput("tagID","",c("All",sort(unique(as.character(tag.data$DETECTEDID)))),multiple = TRUE))
      )
    ), 
  
  mainPanel(
    #  Application title
    titlePanel("Interactive Vemco tag data explorer"),
    # First level text
    h3("Filtering Options"),
        # First level text
    h3("Overview"),
    # Descriptive text
    p("This in an interactive web app for exploring tag detections from a Vemco hydrophone array. 
      Load a standard Vemco 'position' file from your local machine, and use the various sliders to 
      filter the data based on horizontal position error (HPE), number of receiver triangles used
      to estimate the position, etc."),
    # Plot the tag data map
    h4("Tag map"),
    p("A map of all tag detections."),
    plotOutput('map.all'),
    p("A map of all tag detections (gray points) and tag detections after filters have been applied (colored points)."),
    plotOutput('map.filt'),
    # Plot HPE histrograms for each tag
    h4("Horizontal position error (HPE) histrograms"),
    p("Frequency histograms of tag detections after filters have been applied."),
    plotOutput('histHPE')
  )
)

# Define server output #####
server <- function(input, output) {
  # filtered tag data
  tag.data.filt <- reactive(filter(tag.data,HPE <= input$maxHPE,n >= input$minTriangles,DETECTEDID %in% input$tagID))

  # Map of tag detections for all animals, filtered by slider settings
  output$map.filt <- renderPlot({
    tag.data.filt = tag.data.filt()
    ggplot() +
      geom_point(data = tag.data,aes(LON,LAT),colour = 'gray50',alpha = 0.5) +
      geom_point(data = tag.data.filt,aes(LON,LAT,colour = DETECTEDID)) +
      geom_point(data = station.data,aes(LON,LAT),size=2) +
      xlab("\nLongitude (W)") + ylab("Latitude (N)\n") + 
      theme_bw() + theme(plot.background=element_blank(),
                         axis.text.y = element_text(angle = 90, hjust=0.5),
                         panel.spacing = unit(1, "lines")) +
      coord_map()
  })
  
  # Map of tag detections for all animals, filtered by slider settings
  output$map.all <- renderPlot({
    ggplot() +
      # geom_point(data = tag.data,aes(LON,LAT),colour = 'gray50',alpha = 0.5) +
      geom_point(data = tag.data,aes(LON,LAT,colour = DETECTEDID)) +
      geom_point(data = station.data,aes(LON,LAT),size=2) +
      xlab("\nLongitude (W)") + ylab("Latitude (N)\n") + 
      theme_bw() + theme(plot.background=element_blank(),
                         axis.text.y = element_text(angle = 90, hjust=0.5),
                         panel.spacing = unit(1, "lines")) +
      coord_map()
  })
  
  # Histogram of tag HPEs, filtered by slider settings
  output$histHPE <- renderPlot({
    tag.data.filt = tag.data.filt()
    ggplot() + 
      # geom_histogram(data = df,aes(HPE),colour = 'gray50') + 
      geom_histogram(data = tag.data.filt,aes(HPE),colour = 'black') + 
      facet_wrap(~DETECTEDID,scales = "free_y") +
      xlab("\nHorizontal Position Error") + ylab("Frequency\n") +
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

