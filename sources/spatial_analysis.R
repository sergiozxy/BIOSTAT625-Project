library(dplyr)
library(data.table)
library(zoo)
library(bit64)
library(ggplot2)
library(sf)
library(spdep)

setwd("/home/xuyuan/Desktop/2024 fall/BIOSTAT625-Project")

final_output_for_ml <- "cleaned_data_final.csv"
data <- read.csv(final_output_for_ml)

aggregated_data <- data %>%
  group_by(year, `State.Code`) %>%
  summarise(
    # Averaging cost-to-revenue ratio
    avg_cost_to_revenue_ratio = mean(`Cost.to.Revenue.Ratio`, na.rm = TRUE),
    
    # Total discharges
    total_discharges = sum(`Total.Discharges..V...XVIII...XIX...Unknown.`, na.rm = TRUE),
    
    # Hospital total days
    total_hospital_days = sum(`Hospital.Total.Days..V...XVIII...XIX...Unknown..For.Adults...Peds`, na.rm = TRUE),
    
    # Total salaries
    total_salaries = sum(`Total.Salaries.From.Worksheet.A`, na.rm = TRUE),
    
    # Inpatient and outpatient charges
    total_inpatient_charges = sum(`Inpatient.Total.Charges`, na.rm = TRUE),
    total_outpatient_charges = sum(`Outpatient.Total.Charges`, na.rm = TRUE),
    
    # Income and other income
    total_income = sum(`Total.Income`, na.rm = TRUE),
    total_other_income = sum(`Total.Other.Income`, na.rm = TRUE),
    
    # Liabilities, assets, and balances
    total_liabilities = sum(`Total.Liabilities.and.Fund.Balances`, na.rm = TRUE),
    total_accounts_payable = sum(`Accounts.Payable`, na.rm = TRUE),
    total_current_assets = sum(`Total.Current.Assets`, na.rm = TRUE),
    total_fixed_assets = sum(`Total.Fixed.Assets`, na.rm = TRUE),
    total_general_fund_balance = sum(`General.Fund.Balance`, na.rm = TRUE),
    
    # Inventory and patient revenue
    total_inventory = sum(Inventory, na.rm = TRUE),
    total_patient_revenue = sum(`Total.Patient.Revenue`, na.rm = TRUE),
    
    # Number of beds
    total_beds = sum(`Number.of.Beds`, na.rm = TRUE),
    
    .groups = "drop" # Ungroup the data after summarisation
  )

# Create a lookup table for state codes and state names
state_code_name <- data.frame(
  State.Code = c(
    "AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", 
    "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
    "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", 
    "VI", "VT", "WA", "WI", "WV", "WY", "AS"
  ),
  State.Name = c(
    "Alaska", "Alabama", "Arkansas", "Arizona", "California", "Colorado", "Connecticut", "District of Columbia", 
    "Delaware", "Florida", "Georgia", "Guam", "Hawaii", "Iowa", "Idaho", "Illinois", "Indiana", "Kansas", 
    "Kentucky", "Louisiana", "Massachusetts", "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", 
    "Mississippi", "Montana", "North Carolina", "North Dakota", "Nebraska", "New Hampshire", "New Jersey", 
    "New Mexico", "Nevada", "New York", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", 
    "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Virgin Islands", 
    "Vermont", "Washington", "Wisconsin", "West Virginia", "Wyoming", "American Samoa"
  )
)

aggregated_data <- aggregated_data %>%
  left_join(state_code_name, by = "State.Code")


# Load state shapefile/GeoJSON
state_shapefile <- st_read("geodata/us-states.json")

# Merge with aggregated data
spatial_data <- state_shapefile %>%
  left_join(aggregated_data, by = c("State.Code" = "State.Code"))

spatial_data <- spatial_data %>%
  mutate(centroid = st_centroid(geometry))
