# replace this with your directory
# setwd("/home/xuyuan/Desktop/2024 fall/BIOSTAT625-Project")
# setwd("E:/umich/BIOSTAT625-Project")

# we first directly merge the data together:
library(dplyr)
library(data.table)
library(zoo)
library(bit64)
library(ggplot2)
# directly path:
data_dir <- "Hospital Provider Cost Report"

# Generate the list of file paths for the years 2011 to 2022
file_info <- data.frame(
  file_path = sprintf("%s/%d/CostReport_%d_Final.csv", data_dir, 2011:2022, 2011:2022),
  year = 2011:2022
)

# Read all files and merge them into a single data.table
merged_data <- rbindlist(
  lapply(1:nrow(file_info), function(i) {
    fread(file_info$file_path[i])[, year := file_info$year[i]]
  }),
  use.names = TRUE,
  fill = TRUE
)

# Preview the merged data
print(merged_data)

na_counts <- colSums(is.na(merged_data))
print(na_counts / nrow(merged_data))

# note book for the variables
# some basic information
# rpt_rec_num
# Provider CCN
# Hospital Name
# Street Address
# City
# State Code
# Zip Code
# County
# CCN Facility Type # this is a category and you need to convert to dummy
# Rural Versus Urban # need clean
# Type of Control
# Provider Type
# Total Discharges (V + XVIII + XIX + Unknown) # also need to clean
# Hospital Total Days (V + XVIII + XIX + Unknown) For Adults &Peds
# Total Salaries From Worksheet A
# Total Costs
# Inpatient Total Charges
# Outpatient Total Charges
# Total Income
# Total Other Income
# Total Liabilities and Fund Balances
# Accounts Payable
# Total Current Assets
# Total Fixed Assets
# General Fund Balance
# Inventory
# Total Patient Revenue

selected_columns <- c(
  "rpt_rec_num",
  "Provider CCN",
  "Hospital Name",
  "Street Address",
  "City",
  "State Code",
  "Zip Code",
  "County",
  "CCN Facility Type",
  "Rural Versus Urban",
  "Type of Control",
  "Provider Type",
  "Total Discharges (V + XVIII + XIX + Unknown)",
  "Hospital Total Days (V + XVIII + XIX + Unknown) For Adults & Peds",
  "Total Salaries From Worksheet A",
  "Total Costs",
  "Inpatient Total Charges",
  "Outpatient Total Charges",
  "Total Income",
  "Total Other Income",
  "Total Liabilities and Fund Balances",
  "Accounts Payable",
  "Total Current Assets",
  "Total Fixed Assets",
  "General Fund Balance",
  "Inventory",
  "Total Patient Revenue",
  "Number of Beds",
  "year" # Include the year
)

# Keep only selected columns
cleaned_data <- merged_data[, ..selected_columns]

cleaned_data <- cleaned_data %>%
  mutate(across(`CCN Facility Type`, ~ factor(.))) %>%
  bind_cols(model.matrix(~ `CCN Facility Type` - 1, data = .)) %>%
  select(-`CCN Facility Type`)

# setorder(cleaned_data, `Provider CCN`, year)

# Generate Cost-to-Revenue Ratio and Revenue per Bed

# Generate Cost-to-Revenue Ratio
cleaned_data[, `Cost-to-Revenue Ratio` := `Total Costs` / `Total Patient Revenue`]
# Generate Revenue per Bed
cleaned_data[, `Revenue per Bed` := `Total Patient Revenue` / `Number of Beds`]

duplicates <- cleaned_data %>%
  group_by(`Provider CCN`, year) %>%
  filter(n() > 1)

print(nrow(duplicates))


cleaned_data <- cleaned_data %>%
  group_by(`Provider CCN`) %>%
  # Check if ANY numeric variable is fully missing within the group
  filter(!any(sapply(across(where(is.numeric)), function(col) all(is.na(col))))) %>%
  ungroup()

setDT(cleaned_data)

# Aggregate duplicates by taking the mean of numeric values
cleaned_data <- cleaned_data[, lapply(.SD, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else x[1]),
                             by = .(`Provider CCN`, year)]

# cleaned_data <- cleaned_data %>%
#   filter(`Provider CCN` != 512005)

# problematic_data <- cleaned_data %>%
#   filter(`Provider CCN` == 10008)

# na_counts <- colSums(is.na(cleaned_data))
# print(na_counts / nrow(cleaned_data))

cleaned_data2 <- cleaned_data %>%
  group_by(`Provider CCN`) %>%
  mutate(across(
    where(is.numeric),
    ~ if (n() > 2 && sum(!is.na(.)) > 1) {
      interpolated <- zoo::na.approx(., x = year, na.rm = FALSE, rule = 2)
      # Preserve the original data type
      if (inherits(., "integer64")) bit64::as.integer64(interpolated) else interpolated
    } else .
  )) %>%
  ungroup()

setorder(cleaned_data2, `Provider CCN`, year)

# na_counts <- colSums(is.na(cleaned_data2))
# print(na_counts / nrow(cleaned_data2))

cleaned_data2 <- na.omit(cleaned_data2)

# now we can conduct a base line regression model
cleaned_data2 <- cleaned_data2 %>%
  mutate(
    year = as.factor(year),
    `State Code` = as.factor(`State Code`)
  )

cleaned_data2 <- cleaned_data2 %>%
  mutate(across(
    where(~ inherits(., "integer64")),
    as.numeric
  ))

cleaned_data_final <- cleaned_data2
# Define the file path where the CSV file will be saved
output_file <- "cleaned_data_final_temp.csv"

# Save the data frame to a CSV file
write.csv(cleaned_data_final, file = output_file, row.names = FALSE)
# Print a confirmation message
cat("Data has been successfully saved to", output_file, "\n")


data <- read.csv(output_file)


# we need to remove the outliers, since they are too far away from our concentrated data.
# you can not directly regress the model since the outliers are also influential points.
data_filtered <- data[data$`Cost.to.Revenue.Ratio` <= 100, ]
data_filtered$Revenue.per.Bed <- data_filtered$`Revenue.per.Bed` / 1000000
data_filtered <- data_filtered[data_filtered$`Revenue.per.Bed` <= 100, ]

print(names(data))


# can also use a for loop to do this
# just make it all units to million instead of single

data_filtered$Total.Salaries.From.Worksheet.A <- data_filtered$`Total.Salaries.From.Worksheet.A` / 1000000
data_filtered$Inpatient.Total.Charges <- data_filtered$`Inpatient.Total.Charges` / 1000000
data_filtered$Outpatient.Total.Charges <- data_filtered$`Outpatient.Total.Charges` / 1000000
data_filtered$Total.Income <- data_filtered$`Total.Income` / 1000000
data_filtered$Total.Other.Income <- data_filtered$`Total.Other.Income` / 1000000
data_filtered$Total.Liabilities.and.Fund.Balances <- data_filtered$`Total.Liabilities.and.Fund.Balances` / 1000000
data_filtered$Accounts.Payable <- data_filtered$`Accounts.Payable` / 1000000
data_filtered$Total.Current.Assets <- data_filtered$`Total.Current.Assets` / 1000000
data_filtered$Total.Fixed.Assets <- data_filtered$`Total.Fixed.Assets` / 1000000
data_filtered$General.Fund.Balance <- data_filtered$`General.Fund.Balance` / 1000000
data_filtered$Total.Patient.Revenue <- data_filtered$`Total.Patient.Revenue` / 1000000
data_filtered$Total.Patient.Revenue <- data_filtered$`Total.Patient.Revenue` / 1000000


# convert above information to million instead of units

final_output_for_ml <- "cleaned_data_final.csv"
write.csv(data_filtered, file = final_output_for_ml, row.names = FALSE)

