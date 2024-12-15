library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(ggplot2)
library(readr)
library(sf)
memory.limit(size = 16000)  # Adjust as needed
setwd("/home/xuyuan/Desktop/2024 fall/BIOSTAT625-Project/HospitalRevenue_map_ui")
data_file <- "sample4000_analysis_geocode.csv"
if (file.exists(data_file)) {
  sample_analysis_geocode <- readr::read_csv(data_file)
  message("Data loaded successfully from ", data_file)
} else {
  stop("Error: Data file not found. Please ensure ", data_file, " exists.")
}
sample_analysis_geocode <- sample_analysis_geocode %>%
  filter(Revenue.per.Bed <= 15)
state_data <- read.csv("state_aggregated.csv")
state_data <- state_data %>%
  mutate(State.Name = tolower(state.name[match(State.Code, state.abb)])) %>%
  filter(!is.na(State.Name))  # Exclude rows with missing state names

us_map <- maps::map("state", fill = TRUE, plot = FALSE)
us_map_sf <- st_as_sf(us_map, crs = 4326)

us_map_merged <- us_map_sf %>%
  left_join(state_data, by = c("ID" = "State.Name"))

palette_cost <- colorNumeric(palette = "YlOrRd", domain = state_data$AvgCostToRevenue)
palette_revenue <- colorNumeric(palette = "Blues", domain = state_data$AvgRevenuePerBed)




# Ensure 'year' column exists; if not, create one
if (!"year" %in% names(sample_analysis_geocode)) {
  sample_analysis_geocode <- sample_analysis_geocode %>%
    mutate(year = rep(2011:2022, length.out = n()))  # Example year data
}

# Define UI
ui <- fluidPage(
  # Header Section
  titlePanel(
    h1("Interactive US Map for Hospital Financial Performance (2011-2022)", align = "center"),
    windowTitle = "Hospital Financial Performance Analysis"
  ),

  # Project Details
  fluidRow(
    column(12,
           wellPanel(
             h3("Project Objective"),
             p("The goal of this project is to analyze and predict hospital operating costs and revenue trends,
               enabling hospital administrators to make informed strategic decisions.
               Using advanced statistical models and foundational machine learning techniques,
               the project aims to deliver actionable insights into financial performance and future trends.
               Data Source is from the ",
               a("CMS Hospital Provider Cost Report dataset",
                 href = "https://data.cms.gov/provider-compliance/cost-report/hospital-provider-cost-report",
                 target = "_blank"),
               ".")
           )
             # add : https://data.cms.gov/provider-compliance/cost-report/hospital-provider-cost-report




    )
  ),

  # Tabset Panel
  tabsetPanel(
    tabPanel("Hospital-Level Map",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year",
                             "Select Year:",
                             min = 2011,
                             max = 2022,
                             value = 2011,
                             step = 1,
                             sep = ""),
                 checkboxInput("showRevenueLegend", "Show Revenue per Bed Legend", value = TRUE)
               ),
               mainPanel(
                 leafletOutput("hospitalMap", height = 600)
               )
             )),
    tabPanel("State-Level Summary",
             fluidRow(
               column(
                 width = 6,
                 h4("State Map - Avg Revenue per Bed (2011-2022)", align = "center"),
                 leafletOutput("stateMapRevenue", height = 500)
               ),
               column(
                 width = 6,
                 h4("State Map - Avg Cost-to-Revenue Ratio (2011-2022)", align = "center"),
                 leafletOutput("stateMapCost", height = 500)
               )
             )
    )
  ),

  # Footer Section
  fluidRow(
    column(12,
           tags$footer(
             style = "margin-top: 30px; padding: 10px; background-color: #f8f9fa; border-top: 1px solid #e4e4e4;",
             p(
               style = "text-align: center; font-size: 14px;",
               "Project Name: 'Cost Analysis for Hospital Financial Performance in the US (2011-2022)' | ",
               "BIOSTAT 625 Group Project ",
               "Major Contributions by: Charlotte Xu"
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
        values = ~AvgCostToRevenue[!is.na(AvgCostToRevenue)],
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
        values = ~AvgRevenuePerBed[!is.na(AvgRevenuePerBed)],
        position = "bottomright",
        title = "Avg Revenue per Bed"
      )
  })
}

# Run the application
shinyApp(ui, server)
