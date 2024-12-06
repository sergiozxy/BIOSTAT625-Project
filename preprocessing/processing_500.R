# Load necessary libraries
library(dplyr)
library(tidyverse)
library(tidygeocoder)

# Function to clean and preprocess valid hospital addresses
preprocess_hospitals <- function(data) {
  # Step 1: Filter valid addresses
  valid_addresses <- data %>%
    filter(grepl("\\d+.*[A-Za-z]", Street.Address))

  # Step 2: Filter valid hospital names
  valid_hospitals <- valid_addresses %>%
    filter(
      !grepl("^\\d+$", Hospital.Name),  # Exclude names that are only numbers
      nchar(Hospital.Name) > 3          # Exclude abnormally short names
    )

  return(valid_hospitals)
}

# Function to sample and geocode hospitals
generate_sample_geocoded_data <- function(hospitals, sample_size, file_path) {
  # Step 1: Sample hospitals
  sampled_hospitals <- hospitals %>%
    group_by(Hospital.Name) %>%
    summarise(
      Street.Address = first(Street.Address),
      City = first(City),
      State.Code = first(State.Code),
      .groups = "drop"
    ) %>%
    sample_n(sample_size)

  # Step 2: Check if file exists
  if (!file.exists(file_path)) {
    # Geocode if the file does not exist
    geocoded_sample <- sampled_hospitals %>%
      geocode(
        street = Street.Address,
        city = City,
        state = State.Code,
        method = "osm"
      )

    # Save geocoded data to CSV
    write.csv(geocoded_sample, file_path, row.names = FALSE)
    message("Geocoding completed and data saved to ", file_path)
  } else {
    # Load from file if it exists
    geocoded_sample <- read.csv(file_path)
    message("Geocoded data read from existing file: ", file_path)
  }

  # Clean geocoded data
  geocoded_sample_clean <- geocoded_sample %>%
    filter(!is.na(lat) & !is.na(long))

  return(geocoded_sample_clean)
}

# Function to generate analysis datasets
generate_analysis_datasets <- function(valid_hospitals, geocoded_data) {
  # Join geocoded data back to valid hospitals
  updated_hospitals <- valid_hospitals %>%
    inner_join(geocoded_data %>% select(Hospital.Name, lat, long), by = "Hospital.Name")

  # Create hospital-level dataset
  analysis_geocode <- updated_hospitals %>%
    select(Hospital.Name, Street.Address, City, State.Code, lat, long, Revenue.per.Bed, Cost.to.Revenue.Ratio)

  # Create state-level aggregated dataset
  state_aggregated <- updated_hospitals %>%
    group_by(State.Code) %>%
    summarise(
      AvgCostToRevenue = mean(Cost.to.Revenue.Ratio, na.rm = TRUE),
      AvgRevenuePerBed = mean(Revenue.per.Bed, na.rm = TRUE)
    )

  return(list(analysis_geocode = analysis_geocode, state_aggregated = state_aggregated))
}

# Main preprocessing function
preprocess_and_save <- function(data, sample_size, analysis_file, aggregated_file) {
  valid_hospitals <- preprocess_hospitals(data)
  geocoded_data <- generate_sample_geocoded_data(valid_hospitals, sample_size, analysis_file)
  datasets <- generate_analysis_datasets(valid_hospitals, geocoded_data)

  # Save datasets
  save(datasets$analysis_geocode, file = analysis_file)
  save(datasets$state_aggregated, file = aggregated_file)

  # Write datasets to CSV
  write.csv(datasets$analysis_geocode, sub(".RData$", ".csv", analysis_file), row.names = FALSE)
  write.csv(datasets$state_aggregated, sub(".RData$", ".csv", aggregated_file), row.names = FALSE)

  return(datasets)
}

# Example execution
# Replace `data_filtered` with your actual input data
# datasets <- preprocess_and_save(data_filtered, 500, "sample50_analysis_geocode.RData", "sample50_state_aggregated.RData")
