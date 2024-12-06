library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(ggplot2)
library(readr)
library(sf)
memory.limit(size = 16000)  # Adjust as needed

data_file <- "sample4000_analysis_geocode.csv"
if (file.exists(data_file)) {
  sample_analysis_geocode <- readr::read_csv(data_file)
  message("Data loaded successfully from ", data_file)
} else {
  stop("Error: Data file not found. Please ensure ", data_file, " exists.")
}
sample_analysis_geocode <- sample_analysis_geocode %>%
  filter(Revenue.per.Bed <= 15)

# state_file <- "state_aggregated.csv"
# # state_file <- 'sample50_state_aggregated.csv'
# if (file.exists(state_file)) {
#   state_aggregate <- readr::read_csv(state_file)
#   message("Data loaded successfully from ", state_file)
# } else {
#   stop("Error: Data file not found. Please ensure ", state_file, " exists.")
# }


# Ensure 'year' column exists; if not, create one
if (!"year" %in% names(sample_analysis_geocode)) {
  sample_analysis_geocode <- sample_analysis_geocode %>%
    mutate(year = rep(2011:2022, length.out = n()))  # Example year data
}

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Hospital Map"),
  tabsetPanel(
    tabPanel("Hospital-Level Map",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year",
                             "Select Year:",
                             min = min(sample_analysis_geocode$year, na.rm = TRUE),
                             max = max(sample_analysis_geocode$year, na.rm = TRUE),
                             value = min(sample_analysis_geocode$year, na.rm = TRUE),
                             step = 1,
                             sep = ""),
                 # checkboxInput("showRevenueLegend", "Show Revenue per Bed Legend", value = TRUE),
                 # checkboxInput("showCostLegend", "Show Cost-to-Revenue Ratio Legend", value = TRUE)
               ),
               mainPanel(
                 leafletOutput("hospitalMap")
               )
             )),
    tabPanel("State-Level Summary",
             fluidRow(
               column(
                 width = 6,
                 leafletOutput("stateMapRevenue", height = 500)
               ),
               column(
                 width = 6,
                 leafletOutput("stateMapCost", height = 500)
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive dataset filtered by year
  filtered_data <- reactive({
    req(sample_analysis_geocode)
    sample_analysis_geocode %>%
      filter(year == input$year) %>%
      filter(!is.na(lat), !is.na(long))
  })

  output$hospitalMap <- renderLeaflet({
    req(filtered_data())

    # Define a color palette for Revenue per Bed
    pal <- colorNumeric(palette = "viridis", domain = filtered_data()$Revenue.per.Bed)

    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~long, ~lat,
        radius = ~ifelse(Cost.to.Revenue.Ratio < 0.5, 1.25,
                         ifelse(Cost.to.Revenue.Ratio < 1, 3, 3)),
        color = ~pal(Revenue.per.Bed),  # Fill color based on Revenue per Bed
        stroke = TRUE,                  # Enable border
        weight = 2,                     # Border thickness
        opacity = 1,                    # Border opacity
        dashArray = ~ifelse(Cost.to.Revenue.Ratio > 0.5, "0", "4 2"), # Solid or dashed
        fillOpacity = 0.8,              # Opacity of the fill
        popup = ~paste0(
          "<b>Hospital:</b> ", Hospital.Name, "<br>",
          "<b>Address:</b> ", Street.Address, "<br>",
          "<b>City:</b> ", City, "<br>",
          "<b>State:</b> ", State.Code, "<br>",
          "<b>Revenue per Bed:</b> $", Revenue.per.Bed, "<br>",
          "<b>Cost-to-Revenue Ratio:</b> ", Cost.to.Revenue.Ratio
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~Revenue.per.Bed,
        title = "Revenue per Bed",
        opacity = ifelse(input$showRevenueLegend, 1, 0)
      ) %>%
    addControl(
      html = "
    <div style='background: white; padding: 10px; border-radius: 5px;'>
      <b>Cost-to-Revenue Ratio</b><br>
      <div style='display: flex; align-items: center; margin-bottom: 5px;'>
        <svg height='12' width='12'>
          <circle cx='6' cy='6' r='3' style='fill:none;stroke:black;stroke-width:2;' />
        </svg>
        <span style='margin-left: 10px;'>Solid (> 0.5)</span>
      </div>
      <div style='display: flex; align-items: center;'>
        <svg height='24' width='24'>
          <circle cx='12' cy='12' r='6' style='fill:none;stroke:black;stroke-dasharray:4,2;stroke-width:2;' />
        </svg>
        <span style='margin-left: 10px;'>Dashed (≤ 0.5)</span>
      </div>
    </div>",
      position = "topright"
    )
  })


  # output$stateMapRevenue <- renderLeaflet({
  #   req(test_data)  # Use subset of data
  #   leaflet(test_data) %>%
  #     addTiles() %>%
  #     addPolygons(
  #       lng = ~long, lat = ~lat, group = ~group,
  #       fillColor = ~fillColorRevenue,
  #       fillOpacity = 0.8,
  #       color = "black", # Boundary color
  #       weight = 1,      # Boundary thickness
  #       popup = ~paste(
  #         "<b>State:</b>", State.Code, "<br>",
  #         "<b>Average Revenue per Bed:</b>", round(AvgRevenuePerBed, 2), "<br>",
  #         "<b>Number of Hospitals:</b>", NumberOfHospitals
  #       )
  #     ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       pal = colorNumeric(
  #         palette = "Blues",
  #         domain = state_map_data$AvgRevenuePerBed
  #       ),
  #       values = state_map_data$AvgRevenuePerBed,
  #       title = "Avg Revenue per Bed",
  #       opacity = 1
  #     )
  # })

  # # Render Cost-to-Revenue Ratio Map
  # output$stateMapCost <- renderLeaflet({
  #   leaflet(state_map_data) %>%
  #     addTiles() %>%
  #     addPolygons(
  #       lng = ~long, lat = ~lat, group = ~group,
  #       fillColor = ~fillColorCost,
  #       fillOpacity = 0.8,
  #       color = "black", # Boundary color
  #       weight = 1,      # Boundary thickness
  #       popup = ~paste(
  #         "<b>State:</b>", State.Code, "<br>",
  #         "<b>Average Cost-to-Revenue Ratio:</b>", round(AvgCostToRevenue, 2), "<br>",
  #         "<b>Number of Hospitals:</b>", NumberOfHospitals
  #       )
  #     ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       pal = colorNumeric(
  #         palette = "Reds",
  #         domain = state_map_data$AvgCostToRevenue
  #       ),
  #       values = state_map_data$AvgCostToRevenue,
  #       title = "Avg Cost-to-Revenue Ratio",
  #       opacity = 1
  #     )
  # })

  # Read the CSV data
  state_data <- read.csv("state_aggregated.csv")

  # Add full state names to the data
  state_data <- state_data %>%
    mutate(State.Name = tolower(state.name[match(State.Code, state.abb)])) %>%
    filter(!is.na(State.Name))  # Exclude rows with missing state names

  # Load US states shapefile using `maps` and convert to `sf` object
  us_map <- map("state", fill = TRUE, plot = FALSE)
  us_map_sf <- st_as_sf(us_map, crs = 4326)

  # Join state data with map data
  us_map_merged <- us_map_sf %>%
    left_join(state_data, by = c("ID" = "State.Name"))

  # Define color palettes
  palette_cost <- colorNumeric(palette = "YlOrRd", domain = state_data$AvgCostToRevenue)
  palette_revenue <- colorNumeric(palette = "Blues", domain = state_data$AvgRevenuePerBed)

  # Render the map for AvgCostToRevenue
  output$stateMapCost <- renderLeaflet({
    leaflet(us_map_merged) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~palette_cost(AvgCostToRevenue),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<b>", ID, "</b><br>",
          "Avg Cost to Revenue: ", round(AvgCostToRevenue, 2), "<br>",
          "Number of Hospitals: ", NumberOfHospitals
        )
      ) %>%
      addLegend(
        pal = palette_cost,
        values = ~AvgCostToRevenue,
        position = "bottomright",
        title = "Avg Cost to Revenue"
      )
  })

  # Render the map for AvgRevenuePerBed
  output$stateMapRevenue <- renderLeaflet({
    leaflet(us_map_merged) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~palette_revenue(AvgRevenuePerBed),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<b>", ID, "</b><br>",
          "Avg Revenue per Bed: ", round(AvgRevenuePerBed, 2), "<br>",
          "Number of Hospitals: ", NumberOfHospitals
        )
      ) %>%
      addLegend(
        pal = palette_revenue,
        values = ~AvgRevenuePerBed,
        position = "bottomright",
        title = "Avg Revenue per Bed"
      )
  })
}

# Run the application
shinyApp(ui, server)
