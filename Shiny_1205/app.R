library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(ggplot2)
library(readr)

# source("preprocessing/processing_500.R")

# datasets <- preprocess_and_save(data_filtered, 1000, "sample_analysis_geocode.RData", "sample_state_aggregated.RData")
# sample_analysis_geocode <- datasets$analysis_geocode
# sample_state_aggregated <- datasets$state_aggregated

data_file <- "sample50_analysis_geocode.csv"
if (file.exists(data_file)) {
  sample_analysis_geocode <- readr::read_csv(data_file)
  message("Data loaded successfully from ", data_file)
} else {
  stop("Error: Data file not found. Please ensure ", data_file, " exists.")
}


# Ensure 'year' column exists; if not, create one
if (!"year" %in% names(sample_analysis_geocode)) {
  sample_analysis_geocode <- sample_analysis_geocode %>%
    mutate(year = rep(2011:2022, length.out = n()))  # Example year data
}
# Get state boundaries
states_df <- map_data("state")

# Create a lookup table for state names and abbreviations
state_conversion <- data.frame(
  State.Name = tolower(state.name),  # Convert to lowercase to match map_data
  State.Code = state.abb
)

# Add state abbreviations to states_df
states_df <- states_df %>%
  rename(State.Name = region) %>%  # Rename 'region' to 'State.Name'
  left_join(state_conversion, by = "State.Name")  # Merge with abbreviations

# View the updated states_df
head(states_df)

# # Merge with sample_state_aggregated
# state_map_data <- states_df %>%
#   left_join(sample_state_aggregated, by = "State.Code")
#
# # View the final merged dataset
# head(state_map_data)

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
                 checkboxInput("showRevenueLegend", "Show Revenue per Bed Legend", value = TRUE),
                 checkboxInput("showCostLegend", "Show Cost-to-Revenue Ratio Legend", value = TRUE)
               ),
               mainPanel(
                 leafletOutput("hospitalMap")
               )
             )),
    tabPanel("State-Level Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("stateVariable",
                             "Select Variable:",
                             choices = c("AvgCostToRevenue" = "AvgCostToRevenue",
                                         "AvgRevenuePerBed" = "AvgRevenuePerBed"),
                             selected = "AvgRevenuePerBed"),
                 checkboxInput("showStateLegend", "Show Legend", value = TRUE)
               ),
               mainPanel(
                 leafletOutput("stateMap")
               )
             ))
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

  # Render hospital-level map
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
      )
  })
  # Render a placeholder map for state-level tab
  output$stateMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = states_df,
        fillColor = "grey",
        fillOpacity = 0.5,
        color = "black",
        weight = 1,
        popup = ~paste("State:", State.Code)
      ) %>%
      addLegend(
        "bottomright",
        colors = "grey",
        labels = "Placeholder Map",
        title = "State Summary"
      )
  })
  # output$stateMap <- renderLeaflet({
  #   req(state_map_data)
  #
  #   variable <- input$stateVariable
  #
  #   # Prepare the data for leaflet
  #   state_map_data_prepared <- state_map_data %>%
  #     mutate(
  #       fillColor = colorNumeric(palette = "Blues", domain = state_map_data[[variable]], na.color = "transparent")(state_map_data[[variable]]),
  #       popupContent = lapply(
  #         seq_len(nrow(state_map_data)),
  #         function(i) {
  #           paste0(
  #             "<b>State:</b> ", state_map_data$State.Code[i], "<br>",
  #             "<b>", variable, ":</b> ", round(state_map_data[[variable]][i], 2)
  #           )
  #         }
  #       )
  #     )
  #
  #   # Create leaflet map
  #   leaflet(state_map_data_prepared) %>%
  #     addTiles() %>%
  #     addPolygons(
  #       lng = ~long, lat = ~lat, group = ~group,
  #       fillColor = ~fillColor,
  #       fillOpacity = 0.8,
  #       color = "black", # Boundary color
  #       weight = 1,      # Boundary thickness
  #       popup = ~popupContent
  #     ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       pal = colorNumeric(palette = "Blues", domain = state_map_data[[variable]], na.color = "transparent"),
  #       values = state_map_data[[variable]],
  #       title = ifelse(variable == "AvgRevenuePerBed",
  #                      "Average Revenue per Bed",
  #                      "Average Cost-to-Revenue Ratio"),
  #       opacity = ifelse(input$showStateLegend, 1, 0)
  #     )
  # })
}

# Run the application
shinyApp(ui, server)
