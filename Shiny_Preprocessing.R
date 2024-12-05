library(dplyr)
library(tidygeocoder)


valid_addresses <- data_filtered %>%
  filter(grepl("\\d+.*[A-Za-z]", Street.Address))



# Step 1: Preprocess Hospital.Name to filter out anomalies
valid_hospitals <- valid_addresses %>%
  filter(
    !grepl("^\\d+$", Hospital.Name),  # Exclude names that are only numbers
    nchar(Hospital.Name) > 3          # Exclude abnormally short names
  )

# Step 2: Sample 500 hospitals for geocoding
sampled_hospitals <- valid_hospitals %>%
  group_by(Hospital.Name) %>%
  summarise(
    Street.Address = first(Street.Address),
    City = first(City),
    State.Code = first(State.Code),
    .groups = "drop"
  ) %>%
  sample_n(500)  # Randomly sample 500 hospitals

# Step 3: Geocode the sampled hospitals
geocoded_sample <- sampled_hospitals %>%
  geocode(
    street = Street.Address,
    city = City,
    state = State.Code,
    method = "osm"
  )

geocoded_sample_clean <- geocoded_sample %>%
  filter(!is.na(lat) & !is.na(long))

# Step 2: Join geocoded_sample back to valid_hospitals
updated_hospitals <- valid_hospitals %>%
  inner_join(geocoded_sample_clean %>% select(Hospital.Name, lat, long), by = c("Hospital.Name"))


# Step 3: Retain only rows with valid geocoded data and include sampled_id
sample50_analysis_geocode <- updated_hospitals %>%
  select(Hospital.Name, Street.Address, City, State.Code, lat, long, Revenue.per.Bed, Cost.to.Revenue.Ratio )
# add variable: FTE - Employees on Payroll



# View the final dataset
head(sample50_analysis_geocode)
save(sample50_analysis_geocode, file = "sample50_analysis_geocode.RData")

write.csv(sample50_analysis_geocode, 'sample50_analysis_geocode.csv')

# Check the number of geocoded hospitals
n_distinct(sample50_analysis_geocode$Hospital.Name)

