
library(dplyr)
library(data.table)
library(zoo)
library(bit64)
library(ggplot2)

setwd("/home/xuyuan/Desktop/2024 fall/BIOSTAT625-Project")
# setwd("E:/umich/BIOSTAT625-Project")

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
model_summary <- summary(baseline_model)

# now report all results to a table and store it in csv and then report using latex
coeff_table <- as.data.frame(model_summary$coefficients)

coeff_table <- cbind(Variable = rownames(coeff_table), coeff_table)

filtered_table <- subset(coeff_table, !grepl("State.Code", Variable))

colnames(filtered_table) <- c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")

output_file <- "./tables/table1.csv"
write.csv(filtered_table, file = output_file, row.names = FALSE)
cat("Filtered and rounded results have been saved to:", output_file, "\n")

# filtered_table[, 2:5] <- lapply(filtered_table[, 2:5], function(x) round(as.numeric(x), 3))

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

# Print the summary of the regression
model_summary <- summary(baseline_model2)

# now report all results to a table and store it in csv and then report using latex
coeff_table <- as.data.frame(model_summary$coefficients)

coeff_table <- cbind(Variable = rownames(coeff_table), coeff_table)

filtered_table <- subset(coeff_table, !grepl("State.Code", Variable))

colnames(filtered_table) <- c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")

output_file <- "./tables/table2.csv"
write.csv(filtered_table, file = output_file, row.names = FALSE)
cat("Filtered and rounded results have been saved to:", output_file, "\n")

