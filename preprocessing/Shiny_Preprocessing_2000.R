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
  sample_n(2000)  # Randomly sample 2000 hospitals

# Step 3: Geocode the sampled hospitals
file_path <- "sample2000_analysis_geocode.csv"

# Check if the file exists
if (!file.exists(file_path)) {
  # If the file does not exist, run the geocoding process
  geocoded_sample <- sampled_hospitals %>%
    geocode(
      street = Street.Address,
      city = City,
      state = State.Code,
      method = "osm"
    )

  # Write the geocoded data to a CSV file
  write.csv(geocoded_sample, file_path, row.names = FALSE)

  message("Geocoding completed and data saved to ", file_path)
} else {
  # If the file exists, read the data from the CSV
  geocoded_sample <- read.csv(file_path)

  message("Geocoded data read from existing file: ", file_path)
}

# Clean the geocoded data
geocoded_sample_clean <- geocoded_sample %>%
  filter(!is.na(lat) & !is.na(long))

# Step 4: Join geocoded_sample back to valid_hospitals
updated_hospitals <- valid_hospitals %>%
  inner_join(geocoded_sample_clean %>% select(Hospital.Name, lat, long), by = c("Hospital.Name"))

# Step 5: Prepare the analysis dataset
sample2000_analysis_geocode <- updated_hospitals %>%
  select(Hospital.Name, Street.Address, City, State.Code, lat, long, Revenue.per.Bed, Cost.to.Revenue.Ratio)
# add variable: FTE - Employees on Payroll

# Step 6: Prepare the state-aggregated dataset
sample2000_state_aggregated <- updated_hospitals %>%
  group_by(State.Code) %>%
  summarise(
    AvgCostToRevenue = mean(Cost.to.Revenue.Ratio, na.rm = TRUE),
    AvgRevenuePerBed = mean(Revenue.per.Bed, na.rm = TRUE)
  )

# Save datasets to RData files
save(sample2000_analysis_geocode, file = "sample2000_analysis_geocode.RData")
save(sample2000_state_aggregated, file = "sample2000_state_aggregated.RData")

# Write datasets to CSV files
write.csv(sample2000_analysis_geocode, 'sample2000_analysis_geocode.csv', row.names = FALSE)
write.csv(sample2000_state_aggregated, 'sample2000_state_aggregated.csv', row.names = FALSE)

# Check the number of geocoded hospitals
n_distinct(sample2000_analysis_geocode$Hospital.Name)
