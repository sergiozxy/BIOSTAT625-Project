# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)

# Assuming sample50_analysis_geocode is already loaded
# Ensure 'year' column exists; if not, create one
if (!"year" %in% names(sample50_analysis_geocode)) {
  sample50_analysis_geocode <- sample50_analysis_geocode %>%
    mutate(year = rep(2011:2022, length.out = n()))  # Example year data
}

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Hospital Map - Revenue per Bed"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Select Year:",
                  min = min(sample50_analysis_geocode$year, na.rm = TRUE),
                  max = max(sample50_analysis_geocode$year, na.rm = TRUE),
                  value = min(sample50_analysis_geocode$year, na.rm = TRUE),
                  step = 1,
                  sep = "")
      # checkboxInput("showRevenueLegend", "Show Revenue per Bed Legend", value = TRUE),
      # checkboxInput("showCostLegend", "Show Cost-to-Revenue Ratio Legend", value = TRUE)
    ),
    mainPanel(
      leafletOutput("hospitalMap")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive dataset filtered by year
  filtered_data <- reactive({
    req(sample50_analysis_geocode)
    sample50_analysis_geocode %>%
      filter(year == input$year) %>%
      filter(!is.na(lat), !is.na(long))
  })

  # Render leaflet map
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
        # color = 'black',
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
      # Add Revenue per Bed Legend
      addLegend(
        "bottomright",
        pal = pal,
        values = ~Revenue.per.Bed,
        title = "Revenue per Bed",
        opacity = ifelse(input$showLegend, 1, 0)
      ) %>%
    # Add Custom Cost-to-Revenue Ratio Legend
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
}

# Run the application
shinyApp(ui, server)
