
library(dplyr)
library(data.table)
library(zoo)
library(bit64)
library(ggplot2)

# setwd("/home/xuyuan/Desktop/2024 fall/BIOSTAT625-Project")
setwd("E:/umich/BIOSTAT625-Project")

final_output_for_ml <- "cleaned_data_final.csv"

data <- read.csv(final_output_for_ml)

baseline_formula <- as.formula(
    "`Cost.to.Revenue.Ratio` ~
  `Total.Discharges..V...XVIII...XIX...Unknown.` +
  `Hospital.Total.Days..V...XVIII...XIX...Unknown..For.Adults...Peds` +
  `Total.Salaries.From.Worksheet.A` +
  `Inpatient.Total.Charges` +
  `Outpatient.Total.Charges` +
  `Total.Income` +
  `Total.Other.Income` +
  `Total.Liabilities.and.Fund.Balances` +
  `Accounts.Payable` +
  `Total.Current.Assets` +
  `Total.Fixed.Assets` +
  `General.Fund.Balance` +
  Inventory +
  `Total.Patient.Revenue` +
  `Number.of.Beds` +
  year +
  `State.Code`"
)

variables <- all.vars(baseline_formula)

baseline_model <- lm(baseline_formula, data = data)

# Print the summary of the regression
summary(baseline_model)

# now for the model 2:
baseline_formula2 <- as.formula(
    "`Revenue.per.Bed` ~
  `Total.Discharges..V...XVIII...XIX...Unknown.` +
  `Hospital.Total.Days..V...XVIII...XIX...Unknown..For.Adults...Peds` +
  `Total.Salaries.From.Worksheet.A` +
  `Inpatient.Total.Charges` +
  `Outpatient.Total.Charges` +
  `Total.Income` +
  `Total.Other.Income` +
  `Total.Liabilities.and.Fund.Balances` +
  `Accounts.Payable` +
  `Total.Current.Assets` +
  `Total.Fixed.Assets` +
  `General.Fund.Balance` +
  Inventory +
  `Total.Patient.Revenue` +
  `Number.of.Beds` +
  year +
  `State.Code`"
)
baseline_model2 <- lm(baseline_formula2, data = data)
print(summary(baseline_model2))

