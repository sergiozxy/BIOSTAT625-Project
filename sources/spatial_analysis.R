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
library(spatialreg)

setwd("/home/xuyuan/Desktop/2024 fall/BIOSTAT625-Project")
setwd("E:/umich/BIOSTAT625-Project")
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



# this is running too long I am thinking about how to revise the model

slm_formula <- avg_cost_to_revenue_ratio ~ total_discharges + total_hospital_days + total_salaries

slm_model <- lagsarlm(slm_formula, data = spatial_data, listw = knn_weights, zero.policy = TRUE)
summary(slm_model)


















