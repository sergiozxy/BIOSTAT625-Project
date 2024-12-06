library(dplyr)
library(tidyverse)
library(tidygeocoder)

# Step 1: Preprocess Hospital.Name to filter out anomalies
valid_hospitals <- data_filtered %>%
  filter(grepl("\\d+.*[A-Za-z]", Street.Address)) %>%  # Ensure valid street addresses
  filter(
    !grepl("^\\d+$", Hospital.Name),  # Exclude names that are only numbers
    nchar(Hospital.Name) > 3          # Exclude abnormally short names
  )

# Step 2: Sample 2000 hospitals for geocoding
sampled_hospitals <- valid_hospitals %>%
  group_by(Hospital.Name) %>%
  summarise(
    Street.Address = first(Street.Address),
    City = first(City),
    State.Code = first(State.Code),
    .groups = "drop"
  ) %>%
  sample_n(4000)  # Randomly sample 2000 hospitals

# Step 3: Geocode the sampled hospitals
geocoded_sample <- sampled_hospitals %>%
  geocode(
    street = Street.Address,
    city = City,
    state = State.Code,
    method = "osm"
  )

# Clean the geocoded data
geocoded_sample_clean <- geocoded_sample %>%
  filter(!is.na(lat) & !is.na(long))

# Step 4: Join geocoded_sample back to valid_hospitals
updated_hospitals <- valid_hospitals %>%
  inner_join(geocoded_sample_clean %>% select(Hospital.Name, lat, long), by = c("Hospital.Name"))

# Step 5: Prepare the analysis dataset
sample4000_analysis_geocode <- updated_hospitals %>%
  select(Hospital.Name, Street.Address, City, State.Code, lat, long, Revenue.per.Bed, Cost.to.Revenue.Ratio)
# add variable: FTE - Employees on Payroll

# Step 6: Prepare the state-aggregated dataset
state_aggregated <- data_filtered %>%
  group_by(State.Code) %>%
  summarise(
    AvgCostToRevenue = mean(Cost.to.Revenue.Ratio, na.rm = TRUE),
    AvgRevenuePerBed = mean(Revenue.per.Bed, na.rm = TRUE),
    NumberOfHospitals = n() # Count the number of hospitals in each state
  )

# Save datasets to RData files
# save(sample2000_analysis_geocode, file = "sample2000_analysis_geocode.RData")
# save(sample2000_state_aggregated, file = "sample2000_state_aggregated.RData")

# Write datasets to CSV files
write.csv(sample4000_analysis_geocode, 'sample4000_analysis_geocode.csv', row.names = FALSE)
write.csv(state_aggregated, 'state_aggregated.csv', row.names = FALSE)

# Check the number of geocoded hospitals
# n_distinct(sample2000_analysis_geocode$Hospital.Name)
