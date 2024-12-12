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
        total_discharges = sum(`Total.Discharges..V...XVIII...XIX...Unknown.`, na.rm = TRUE),
        total_hospital_days = sum(`Hospital.Total.Days..V...XVIII...XIX...Unknown..For.Adults...Peds`, na.rm = TRUE),
        total_salaries = sum(`Total.Salaries.From.Worksheet.A`, na.rm = TRUE),
        total_inpatient_charges = sum(`Inpatient.Total.Charges`, na.rm = TRUE),
        total_outpatient_charges = sum(`Outpatient.Total.Charges`, na.rm = TRUE),
        total_income = sum(`Total.Income`, na.rm = TRUE),
        total_other_income = sum(`Total.Other.Income`, na.rm = TRUE),
        total_liabilities = sum(`Total.Liabilities.and.Fund.Balances`, na.rm = TRUE),
        total_accounts_payable = sum(`Accounts.Payable`, na.rm = TRUE),
        total_current_assets = sum(`Total.Current.Assets`, na.rm = TRUE),
        total_fixed_assets = sum(`Total.Fixed.Assets`, na.rm = TRUE),
        total_general_fund_balance = sum(`General.Fund.Balance`, na.rm = TRUE),
        total_inventory = sum(Inventory, na.rm = TRUE),
        total_patient_revenue = sum(`Total.Patient.Revenue`, na.rm = TRUE),
        total_beds = sum(`Number.of.Beds`, na.rm = TRUE),
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

gwr_bandwidth <- bw.gwr(
  formula = avg_cost_to_revenue_ratio ~ ., 
  data = gwr_data, 
  adaptive = TRUE  # Use adaptive bandwidth for varying densities
)


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

global_r_squared <- gwr_model$GW.diagnostic$R2  # Global R²
adjusted_r_squared <- gwr_model$GW.diagnostic$adjR2  # Adjusted R²
aic_value <- gwr_model$GW.diagnostic$AICc  # Corrected Akaike Information Criterion

# Print diagnostic metrics
print(paste("Global R²: ", global_r_squared))
print(paste("Adjusted R²: ", adjusted_r_squared))
print(paste("AICc: ", aic_value))

spatial_data_state$gwr_total_discharges <- local_coefficients$total_discharges

# Plot the spatial variation of the coefficient
library(ggplot2)
ggplot(spatial_data_state) +
  geom_sf(aes(fill = gwr_total_discharges)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "GWR Coefficients for Total Discharges",
       fill = "Coefficient")

# We can also work on the GWR of the county level data
























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

aggregated_data <- data %>%
    group_by(year, State.Code, County) %>% # Group by State and County
    summarise(
        avg_cost_to_revenue_ratio = mean(`Cost.to.Revenue.Ratio`, na.rm = TRUE),
        total_discharges = sum(`Total.Discharges..V...XVIII...XIX...Unknown.`, na.rm = TRUE),
        total_hospital_days = sum(`Hospital.Total.Days..V...XVIII...XIX...Unknown..For.Adults...Peds`, na.rm = TRUE),
        total_salaries = sum(`Total.Salaries.From.Worksheet.A`, na.rm = TRUE),
        total_inpatient_charges = sum(`Inpatient.Total.Charges`, na.rm = TRUE),
        total_outpatient_charges = sum(`Outpatient.Total.Charges`, na.rm = TRUE),
        total_income = sum(`Total.Income`, na.rm = TRUE),
        total_other_income = sum(`Total.Other.Income`, na.rm = TRUE),
        total_liabilities = sum(`Total.Liabilities.and.Fund.Balances`, na.rm = TRUE),
        total_accounts_payable = sum(`Accounts.Payable`, na.rm = TRUE),
        total_current_assets = sum(`Total.Current.Assets`, na.rm = TRUE),
        total_fixed_assets = sum(`Total.Fixed.Assets`, na.rm = TRUE),
        total_general_fund_balance = sum(`General.Fund.Balance`, na.rm = TRUE),
        total_inventory = sum(Inventory, na.rm = TRUE),
        total_patient_revenue = sum(`Total.Patient.Revenue`, na.rm = TRUE),
        total_beds = sum(`Number.of.Beds`, na.rm = TRUE),
        .groups = "drop" # Ungroup after summarisation
    )

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


# Extract centroids for spatial analysis
spatial_data$centroid <- st_centroid(spatial_data$geometry)
centroid_coords <- st_coordinates(spatial_data$centroid)

knn_list <- knearneigh(centroid_coords, k = 5)
knn_weights <- nb2listw(knn2nb(knn_list), style = "W", zero.policy = TRUE)

# Perform Moran's I to check spatial autocorrelation
moran_result <- moran.test(spatial_data$avg_cost_to_revenue_ratio, knn_weights, zero.policy = TRUE)
print(moran_result)

moran.plot(spatial_data$avg_cost_to_revenue_ratio, knn_weights, zero.policy = TRUE)

# Extract coordinates
coords <- st_coordinates(st_centroid(spatial_data$geometry))
spatial_gwr_data <- SpatialPointsDataFrame(
  coords = coords,  # Coordinates
  data = st_drop_geometry(spatial_data),  # Drop geometry to keep attribute data
  proj4string = CRS("+proj=longlat +datum=WGS84")  # Set CRS to WGS84
)


# gwr_bandwidth <- bw.gwr(
#   formula = avg_cost_to_revenue_ratio ~ total_discharges + total_hospital_days + total_salaries, 
#   data = spatial_gwr_data, 
#   adaptive = TRUE  # Use adaptive bandwidth for varying densities
# )

# Adaptive bandwidth: 72 CV score: 524.1586 
print(paste("Selected GWR Bandwidth:", gwr_bandwidth))
# best is 72 and if you want to skip you can directly skip above codes
gwr_bandwidth <- 72

distance_matrix <- gw.dist(dp.locat = coordinates(spatial_gwr_data))

start_time <- Sys.time()
gwr_model <- gwr.basic(
  formula = avg_cost_to_revenue_ratio ~ total_discharges + total_hospital_days + total_salaries, 
  data = spatial_gwr_data, 
  bw = gwr_bandwidth, 
  adaptive = TRUE,
  parallel.method = "omp",  # Enable OpenMP multi-threading
  parallel.arg = 4  # Use 2 threads for testing
)
end_time <- Sys.time()
print(end_time - start_time)

# the running time is 7 minutes

print("GWR Model Results:")
summary(gwr_model)

# Extract local coefficients
local_coefficients <- as.data.frame(gwr_model$SDF)

# Visualize coefficients for 'total_discharges'
spatial_data$gwr_total_discharges <- local_coefficients$total_discharges

library(ggplot2)


spatial_data_filtered <- spatial_data[!spatial_data$STUSPS %in% c("AK", "HI", "MP", "PR", "TT", "GU"), ]


ggplot(spatial_data_filtered) +
  geom_sf(aes(fill = total_salaries)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "GWR Coefficients for Total Discharges (Excluding AK and HI)",
       fill = "Coefficient")

print(unique(spatial_data_filtered$STUSPS))
