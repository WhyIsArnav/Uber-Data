library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(png)
library(grid)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(dplyr)
library(rsconnect)

uber <- readRDS("uber_data.rds")

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Using a theme from shinythemes
  titlePanel("Uber Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      p("This Shiny app displays various analyses of Uber trip data.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Trips by Hour and Month", 
          p("This plot shows the distribution of Uber trips by hour of the day, broken down by month."),
          withSpinner(plotOutput("tripsByMonthHourPlot"))
        ),
        tabPanel(
          "Trips by Hour", 
          p("This plot shows the total number of Uber trips for each hour of the day."),
          withSpinner(plotOutput("tripsByHourPlot"))
        ),
        tabPanel(
          "Trips by Day", 
          p("This plot shows the total number of Uber trips for each day of the month."),
          withSpinner(plotOutput("tripsByDayPlot"))
        ),
        tabPanel(
          "Trips by Month and Day", 
          p("This plot shows the distribution of Uber trips by the day of the week and month."),
          withSpinner(plotOutput("tripsByMonthDayPlot"))
        ),
        tabPanel(
          "Trips by Base and Month", 
          p("This plot shows the number of Uber trips from different bases for each month."),
          withSpinner(plotOutput("tripsByBaseMonthPlot"))
        ),
        tabPanel(
          "Heatmap by Hour and Day", 
          p("This heatmap shows the number of Uber trips for each hour of the day and day of the month."),
          withSpinner(plotOutput("heatmapByHourDay"))
        ),
        tabPanel(
          "Heatmap by Month and Day", 
          p("This heatmap shows the number of Uber trips for each day of the month and month."),
          withSpinner(plotOutput("heatmapByMonthDay"))
        ),
        tabPanel(
          "Heatmap by Base and Day of Week", 
          p("This heatmap shows the number of Uber trips from different bases for each day of the week."),
          withSpinner(plotOutput("heatmapByBaseWeek"))
        ),
        tabPanel(
          "Geospatial Map", 
          p("This map shows the geospatial distribution of Uber trips, with intensity indicating the number of trips."),
          leafletOutput("geospatialMap")
        ),
        tabPanel(
          "Data Table", 
          p("This table displays the raw Uber trip data used in the analysis."),
          DTOutput("dataTable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$tripsByMonthHourPlot <- renderPlot({
    img <- readPNG("Trips by hour and month.png")
    grid::grid.raster(img)
  })
  
  output$tripsByHourPlot <- renderPlot({
    img <- readPNG("trips_by_hour_graph.png")
    grid::grid.raster(img)
  })
  
  output$tripsByDayPlot <- renderPlot({
    img <- readPNG("trips_by_day_graph.png")
    grid::grid.raster(img)
  })
  
  output$tripsByMonthDayPlot <- renderPlot({
    img <- readPNG("trips_by_month_day_graph.png")
    grid::grid.raster(img)
  })
  
  output$tripsByBaseMonthPlot <- renderPlot({
    img <- readPNG("trips_by_base_month_graph.png")
    grid::grid.raster(img)
  })
  
  output$heatmapByHourDay <- renderPlot({
    img <- readPNG("trips_by_hour_day_heatmap.png")
    grid::grid.raster(img)
  })
  
  output$heatmapByMonthDay <- renderPlot({
    img <- readPNG("trips_by_month_day_heatmap.png")
    grid::grid.raster(img)
  })
  
  output$heatmapByBaseWeek <- renderPlot({
    img <- readPNG("trips_by_bases_week_heatmap.png")
    grid::grid.raster(img)
  })
  
  output$geospatialMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addHeatmap(
        data = heatmap_data,
        lng = ~Lon, 
        lat = ~Lat,  
        intensity = ~Count, 
        blur = 20,    
        radius = 15,
        minOpacity = 0.1,  
        max = max(heatmap_data$Count)
      ) %>%
      addLegend(
        position = "bottomright",  
        pal = colorNumeric(palette = "viridis", domain = heatmap_data$Count),  
        values = heatmap_data$Count,  
        title = "Number of Trips"
      )
  })
  
  output$dataTable <- renderDT({
    datatable(uber)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
