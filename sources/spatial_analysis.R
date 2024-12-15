library(dplyr)
library(data.table)
library(zoo)
library(bit64)
library(ggplot2)
library(sf)
library(spdep)
library(lwgeom)
library(stringi)
library(tigris)
library(GWmodel)

setwd("/home/xuyuan/Desktop/2024 fall/BIOSTAT625-Project")
# setwd("E:/umich/BIOSTAT625-Project")
final_output_for_ml <- "cleaned_data_final.csv"
data <- read.csv(final_output_for_ml)

aggregated_data_state <- data %>%
  group_by(year, State.Code) %>% # Group by State
  summarise(
    avg_cost_to_revenue_ratio = mean(`Cost.to.Revenue.Ratio`, na.rm = TRUE),
    total_discharges = mean(`Total.Discharges..V...XVIII...XIX...Unknown.`, na.rm = TRUE),
    total_hospital_days = mean(`Hospital.Total.Days..V...XVIII...XIX...Unknown..For.Adults...Peds`, na.rm = TRUE),
    total_salaries = mean(`Total.Salaries.From.Worksheet.A`, na.rm = TRUE),
    total_inpatient_charges = mean(`Inpatient.Total.Charges`, na.rm = TRUE),
    total_outpatient_charges = mean(`Outpatient.Total.Charges`, na.rm = TRUE),
    total_income = mean(`Total.Income`, na.rm = TRUE),
    total_other_income = mean(`Total.Other.Income`, na.rm = TRUE),
    total_liabilities = mean(`Total.Liabilities.and.Fund.Balances`, na.rm = TRUE),
    total_accounts_payable = mean(`Accounts.Payable`, na.rm = TRUE),
    total_current_assets = mean(`Total.Current.Assets`, na.rm = TRUE),
    total_fixed_assets = mean(`Total.Fixed.Assets`, na.rm = TRUE),
    total_general_fund_balance = mean(`General.Fund.Balance`, na.rm = TRUE),
    total_inventory = mean(Inventory, na.rm = TRUE),
    total_patient_revenue = mean(`Total.Patient.Revenue`, na.rm = TRUE),
    total_beds = mean(`Number.of.Beds`, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarisation
  )

states_data <- states(cb = TRUE, year = 2022)

aggregated_data_state <- aggregated_data_state %>%
    mutate(State.Code = toupper(State.Code))

# Join spatial and aggregated data
spatial_data_state <- states_data %>%
    right_join(aggregated_data_state, by = c("STUSPS" = "State.Code"))

# Check for missing values
na_counts <- sapply(spatial_data_state, function(x) sum(is.na(x)))

# Remove missing values
spatial_data_state <- na.omit(spatial_data_state)

spatial_data_state <- spatial_data_state[!spatial_data_state$STUSPS %in% c("AK", "HI", "MP", "PR", "TT", "GU"), ]
# Transform to the desired coordinate system
spatial_data_state <- st_transform(spatial_data_state, crs = 4326)

# Extract centroids for spatial analysis
spatial_data_state$centroid <- st_centroid(spatial_data_state$geometry)
centroid_coords <- st_coordinates(spatial_data_state$centroid)

# Create spatial weights
knn_list_state <- knearneigh(centroid_coords, k = 10)
knn_weights_state <- nb2listw(knn2nb(knn_list_state), style = "W", zero.policy = TRUE)

# Perform Moran's I to check spatial autocorrelation
moran_result_state <- moran.test(spatial_data_state$avg_cost_to_revenue_ratio, knn_weights_state, zero.policy = TRUE)
print(moran_result_state)

moran.plot(spatial_data_state$avg_cost_to_revenue_ratio, knn_weights_state, zero.policy = TRUE)



dependent_var <- spatial_data_state$avg_cost_to_revenue_ratio

# Independent variables
independent_vars <- spatial_data_state %>%
    st_drop_geometry() %>% # Drop the geometry column
    dplyr::select(
        total_discharges, 
        total_hospital_days, 
        total_salaries, 
        total_inpatient_charges, 
        total_outpatient_charges, 
        total_income, 
        total_other_income, 
        total_liabilities, 
        total_accounts_payable, 
        total_current_assets, 
        total_fixed_assets, 
        total_general_fund_balance, 
        total_inventory, 
        total_patient_revenue, 
        total_beds
    ) %>%
    na.omit() # Remove rows with missing values
coords <- st_coordinates(st_centroid(spatial_data_state$geometry))
gwr_data <- cbind(
    avg_cost_to_revenue_ratio = dependent_var,
    as.matrix(independent_vars)
) %>%
    as.data.frame()
gwr_data <- na.omit(gwr_data)

spatial_gwr_data <- SpatialPointsDataFrame(
  coords = coords,  # Coordinates from st_centroid
  data = gwr_data,  # Regression data
  proj4string = CRS("+proj=longlat +datum=WGS84")  # Set CRS to WGS84
)


gwr_bandwidth <- bw.gwr(
  formula = avg_cost_to_revenue_ratio ~ ., 
  data = spatial_gwr_data, 
  adaptive = TRUE  # Use adaptive bandwidth for varying densities
)
gwr_model <- gwr.basic(
  formula = avg_cost_to_revenue_ratio ~ ., 
  data = spatial_gwr_data, 
  bw = gwr_bandwidth, 
  adaptive = TRUE  # Adaptive kernel
)

print("GWR Model Results:")
summary(gwr_model)

local_coefficients <- as.data.frame(gwr_model$SDF)

aic_value <- gwr_model$GW.diagnostic$AICc  # Corrected Akaike Information Criterion

# Print diagnostic metrics
print(paste("AICc: ", aic_value))

spatial_data_state$gwr_total_discharges <- local_coefficients$total_discharges

# Plot the spatial variation of the coefficient
library(ggplot2)
result1 <- ggplot(spatial_data_state) +
  geom_sf(aes(fill = gwr_total_discharges)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "GWR Coefficients for Total Discharges",
       fill = "Coefficient")

ggsave(
  filename = "./figures/GWR_State_Total_Discharges.png",
  plot = result1,  # Pass the plot object
  width = 8, height = 6, dpi = 150
)

# We can also work on the GWR of the county level data

final_output_for_ml <- "cleaned_data_final.csv"
data <- read.csv(final_output_for_ml)

aggregated_data <- data %>%
  group_by(year, State.Code, County) %>% # Group by State and County
  summarise(
    avg_cost_to_revenue_ratio = mean(`Cost.to.Revenue.Ratio`, na.rm = TRUE),
    total_discharges = mean(`Total.Discharges..V...XVIII...XIX...Unknown.`, na.rm = TRUE),
    total_hospital_days = mean(`Hospital.Total.Days..V...XVIII...XIX...Unknown..For.Adults...Peds`, na.rm = TRUE),
    total_salaries = mean(`Total.Salaries.From.Worksheet.A`, na.rm = TRUE),
    total_inpatient_charges = mean(`Inpatient.Total.Charges`, na.rm = TRUE),
    total_outpatient_charges = mean(`Outpatient.Total.Charges`, na.rm = TRUE),
    total_income = mean(`Total.Income`, na.rm = TRUE),
    total_other_income = mean(`Total.Other.Income`, na.rm = TRUE),
    total_liabilities = mean(`Total.Liabilities.and.Fund.Balances`, na.rm = TRUE),
    total_accounts_payable = mean(`Accounts.Payable`, na.rm = TRUE),
    total_current_assets = mean(`Total.Current.Assets`, na.rm = TRUE),
    total_fixed_assets = mean(`Total.Fixed.Assets`, na.rm = TRUE),
    total_general_fund_balance = mean(`General.Fund.Balance`, na.rm = TRUE),
    total_inventory = mean(Inventory, na.rm = TRUE),
    total_patient_revenue = mean(`Total.Patient.Revenue`, na.rm = TRUE),
    total_beds = mean(`Number.of.Beds`, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarisation
  )

aggregated_data <- aggregated_data[!aggregated_data$State.Code %in% c("AK", "HI", "MP", "PR", "TT", "GU"), ]

counties_data <- counties(cb = TRUE, year = 2022)

counties_data <- counties_data %>%
    mutate(STATE_NAME = toupper(STATE_NAME))
counties_data <- counties_data %>%
    mutate(NAME = toupper(NAME))

aggregated_data <- aggregated_data %>%
    mutate(County = toupper(County))
aggregated_data <- aggregated_data %>%
    mutate(State.Code = toupper(State.Code))
aggregated_data <- aggregated_data[aggregated_data$County != "NONE", ]
aggregated_data <- aggregated_data[aggregated_data$State.Code != "NONE", ]
print(names(counties_data))

counties_data <- counties_data %>%
    group_by(STUSPS, NAME) %>%
    filter(row_number() == 1) %>%
    ungroup()

spatial_data <- counties_data %>%
    right_join(aggregated_data, by = c("STUSPS" = "State.Code", "NAME" = "County"))

na_counts <- sapply(spatial_data, function(x) sum(is.na(x)))

spatial_data <- na.omit(spatial_data)

spatial_data <- st_transform(spatial_data, crs = 4326)

spatial_data$year <- as.factor(spatial_data$year)

gwr_results <- list()

years <- unique(spatial_data$year)
for (yr in years) {
  cat("Processing year:", yr, "\n")
  # Subset data for the year
  spatial_data_year <- spatial_data[spatial_data$year == yr, ]
  
  # Extract centroids and coordinates
  spatial_data_year$centroid <- st_centroid(spatial_data_year$geometry)
  coords_year <- st_coordinates(spatial_data_year$centroid)
  
  # Create SpatialPointsDataFrame for GWR
  spatial_gwr_data_year <- SpatialPointsDataFrame(
    coords = coords_year,
    data = st_drop_geometry(spatial_data_year),
    proj4string = CRS("+proj=longlat +datum=WGS84")
  )
  
  gwr_bandwidth <- bw.gwr(
    formula = avg_cost_to_revenue_ratio ~ total_discharges + total_hospital_days + total_salaries + total_inventory, 
    data = spatial_gwr_data_year, 
    adaptive = TRUE  # Use adaptive bandwidth for varying densities
  )
  
  print(paste("Selected GWR Bandwidth:", gwr_bandwidth))
  
  # Perform GWR
  gwr_model <- gwr.basic(
    formula = avg_cost_to_revenue_ratio ~ total_discharges + total_hospital_days + total_salaries + total_inventory, 
    data = spatial_gwr_data_year, 
    bw = gwr_bandwidth,  # can also use Preselected bandwidth
    adaptive = TRUE
  )
  
  gwr_results[[as.character(yr)]] <- gwr_model
  
  # Extract and store local coefficients for visualization
  local_coefficients <- as.data.frame(gwr_model$SDF)
  spatial_data_year$gwr_total_discharges <- local_coefficients$total_discharges
  
  gwr_plot <- ggplot(spatial_data_year) +
    geom_sf(aes(fill = gwr_total_discharges)) +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(
      title = paste("GWR Coefficients for Total Discharges (Year:", yr, ")"),
      fill = "Coefficient"
    )
  
  ggsave(
    filename = paste0("./figures/GWR_Total_Discharges_", yr, ".png"),
    plot = gwr_plot,  # Pass the plot object
    width = 8, height = 6, dpi = 150
  )
}

# Summary of GWR results for all years
lapply(gwr_results, summary)
