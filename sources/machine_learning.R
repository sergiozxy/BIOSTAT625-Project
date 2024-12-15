library(dplyr)
library(data.table)
library(zoo)
library(bit64)
library(ggplot2)
library(randomForest)
library(tibble)
library(ranger)
library(xgboost)
library(e1071)
library(lime)
library(kknn)
library(caret)
library(parallel)
library(doParallel)

setwd("/home/xuyuan/Desktop/2024 fall/BIOSTAT625-Project")

# directly path:
data_dir = "Hospital Provider Cost Report"

# Generate the list of file paths for the years 2011 to 2022
file_info = data.frame(
  file_path = sprintf("%s/%d/CostReport_%d_Final.csv", data_dir, 2011:2022, 2011:2022),
  year = 2011:2022
)

# Read all files and merge them into a single data.table
merged_data = rbindlist(
  lapply(1:nrow(file_info), function(i) {
    fread(file_info$file_path[i])[, year := file_info$year[i]]
  }),
  use.names = TRUE,
  fill = TRUE
)

# Preview the merged data
#print(merged_data)

na_counts = colSums(is.na(merged_data))
na_proportion = print(na_counts / nrow(merged_data))
selected_columns = names(na_proportion[na_proportion < 0.2])

selected_columns = 
  selected_columns[-c(1,2,3,4,5,6,7,8,9,11,14,15,17,18,22,23,26,27,31,32,70,71)]


cleaned_data = merged_data[, ..selected_columns]
cleaned_data = na.omit(cleaned_data)
cleaned_data$`Cost-to-Revenue Ratio` = cleaned_data$`Total Costs` / cleaned_data$`Total Patient Revenue`
colnames(cleaned_data)[colnames(cleaned_data) == "Cost-to-Revenue Ratio"] = "CostToRevenueRatio"
cleaned_data$`Rural Versus Urban` = as.factor(cleaned_data$`Rural Versus Urban`)
cleaned_data$`Provider Type` = as.factor(cleaned_data$`Provider Type`)
cleaned_data$`Type of Control` = as.factor(cleaned_data$`Type of Control`)
# cleaned_data[, (4:ncol(cleaned_data)) := lapply(.SD, as.numeric), .SDcols = 4:ncol(cleaned_data)]
colnames(cleaned_data) = make.names(colnames(cleaned_data))

cleaned_data = cleaned_data[,c(1,2,3,4,5,6,8,21,23,25,26,30,38,50)]

numeric_data = cleaned_data[,-c(1,2,3,14)]
numeric_data = as.data.frame(lapply(numeric_data, as.numeric))
other_data = cleaned_data[,c(1,2,3,14)]
numeric_data_scale = scale(numeric_data)
cleaned_data_scale = cbind(other_data,numeric_data_scale)


folds = createFolds(cleaned_data_scale$CostToRevenueRatio, k = 5, list = TRUE, returnTrain = TRUE)


set.seed(625)

train_indices = sample(1:nrow(cleaned_data_scale), 0.8 * nrow(cleaned_data_scale))  # 80% 训练集
train_data = cleaned_data_scale[train_indices, ]
test_data = cleaned_data_scale[-train_indices, ]


# Function to calculate R2
calculate_r2 <- function(true, predicted) {
  ss_residuals <- sum((true - predicted)^2)
  ss_total <- sum((true - mean(true))^2)
  r2 <- 1 - (ss_residuals / ss_total)
  return(r2)
}

calculate_rmse <- function(true, predicted) {
  sqrt(mean((true - predicted)^2))
}

# Initialize result storage
results_r2 <- list(
  knn_gaussian = c(),
  knn_rectangular = c(),
  knn_triangular = c(),
  svm_linear = c(),
  svm_polynomial = c(),
  svm_radial = c(),
  rf_impurity = c(),
  rf_permutation = c()
)

results_rmse <- list(
  knn_gaussian = c(),
  knn_rectangular = c(),
  knn_triangular = c(),
  svm_linear = c(),
  svm_polynomial = c(),
  svm_radial = c(),
  rf_impurity = c(),
  rf_permutation = c()
)
# 5-Fold Cross-Validation
for (i in 1:5) {
  
  train_data <- cleaned_data_scale[folds[[i]], ]
  test_data <- cleaned_data_scale[-folds[[i]], ]
  
  # KNN Gaussian
  knn_model <- kknn(CostToRevenueRatio ~ ., train_data, test_data, k = 5, kernel = "gaussian")
  predictions <- fitted(knn_model)
  results_r2$knn_gaussian <- c(results_r2$knn_gaussian, calculate_r2(test_data$CostToRevenueRatio, predictions))
  results_rmse$knn_gaussian <- c(results_rmse$knn_gaussian, calculate_rmse(test_data$CostToRevenueRatio, predictions))
  
  # KNN Rectangular
  knn_model <- kknn(CostToRevenueRatio ~ ., train_data, test_data, k = 5, kernel = "rectangular")
  predictions <- fitted(knn_model)
  results_r2$knn_rectangular <- c(results_r2$knn_rectangular, calculate_r2(test_data$CostToRevenueRatio, predictions))
  results_rmse$knn_rectangular <- c(results_rmse$knn_rectangular, calculate_rmse(test_data$CostToRevenueRatio, predictions))
  
  # KNN Triangular
  knn_model <- kknn(CostToRevenueRatio ~ ., train_data, test_data, k = 5, kernel = "triangular")
  predictions <- fitted(knn_model)
  results_r2$knn_triangular <- c(results_r2$knn_triangular, calculate_r2(test_data$CostToRevenueRatio, predictions))
  results_rmse$knn_triangular <- c(results_rmse$knn_triangular, calculate_rmse(test_data$CostToRevenueRatio, predictions))
  
  cat("Finish the KNN Result\n")
  
  cl <- makeCluster(6) # Allocate three cores for parallel execution
  registerDoParallel(cl)
  
  # Parallelized SVM computation for different kernels
  svm_results <- foreach(kernel = c("linear", "polynomial", "radial"), .combine = list, .multicombine = TRUE) %dopar% {
    library(e1071)
    svm_model <- svm(CostToRevenueRatio ~ ., data = train_data, type = "eps-regression", kernel = kernel)
    predictions <- predict(svm_model, test_data)
    list(
      kernel = kernel,
      r2 = calculate_r2(test_data$CostToRevenueRatio, predictions),
      rmse = calculate_rmse(test_data$CostToRevenueRatio, predictions)
    )
  }
  
  stopCluster(cl)
  
  # Store results in respective lists
  results_r2$svm_linear <- c(results_r2$svm_linear, svm_results[[1]]$r2)
  results_rmse$svm_linear <- c(results_rmse$svm_linear, svm_results[[1]]$rmse)
  
  results_r2$svm_polynomial <- c(results_r2$svm_polynomial, svm_results[[2]]$r2)
  results_rmse$svm_polynomial <- c(results_rmse$svm_polynomial, svm_results[[2]]$rmse)
  
  results_r2$svm_radial <- c(results_r2$svm_radial, svm_results[[3]]$r2)
  results_rmse$svm_radial <- c(results_rmse$svm_radial, svm_results[[3]]$rmse)
  
  cat("Finish SVM Result\n")
  
  # Random Forest with impurity importance
  rf_model <- ranger(CostToRevenueRatio ~ ., data = train_data, importance = "impurity", num.threads = 4)
  predictions <- predict(rf_model, data = test_data)$predictions
  results_r2$rf_impurity <- c(results_r2$rf_impurity, calculate_r2(test_data$CostToRevenueRatio, predictions))
  results_rmse$rf_impurity <- c(results_rmse$rf_impurity, calculate_rmse(test_data$CostToRevenueRatio, predictions))
  
  # Random Forest with permutation importance
  rf_model <- ranger(CostToRevenueRatio ~ ., data = train_data, importance = "permutation", num.threads = 4)
  predictions <- predict(rf_model, data = test_data)$predictions
  results_r2$rf_permutation <- c(results_r2$rf_permutation, calculate_r2(test_data$CostToRevenueRatio, predictions))
  results_rmse$rf_permutation <- c(results_rmse$rf_permutation, calculate_rmse(test_data$CostToRevenueRatio, predictions))
  
  cat("Finish Random Forest Result\n")
}

# Calculate mean R2 and RMSE for each model
mean_r2 <- sapply(results_r2, mean)
mean_rmse <- sapply(results_rmse, mean)

# Combine and display results
performance_results <- data.frame(
  Model = names(mean_r2),
  Mean_R2 = round(mean_r2, 4),
  Mean_RMSE = round(mean_rmse, 4)
)

print("Model Performance (R2 and RMSE):")
print(performance_results)

output_file <- "./tables/model_performance_results.csv"
write.csv(performance_results, file = output_file, row.names = FALSE)
cat("Results have been saved to:", output_file, "\n")
# > # Print results
#   > print(mean_r2_knn_gaussian)
# [1] 0.7977189
# > print(mean_r2_knn_rectangular)
# [1] 0.7723247
# > print(mean_r2_knn_triangular)
# [1] 0.8261701
# > print(mean_r2_svm_linear)
# [1] 0.3288907
# > print(mean_r2_svm_polynomial)
# [1] -248.4972
# > print(mean_r2_svm_radial)
# [1] 0.5125751
# > print(mean_r2_rf)
# [1] 0.8022499
# > print(mean_r2_rf_per)
# [1] 0.8027581

# then we base on the model to get the best model

train_indices = sample(1:nrow(cleaned_data_scale), 0.8 * nrow(cleaned_data_scale))  # 80% 训练集
train_data = cleaned_data_scale[train_indices, ]
test_data = cleaned_data_scale[-train_indices, ]


best_rf_model <- ranger(
  CostToRevenueRatio ~ ., 
  data = train_data, 
  num.trees = 500, 
  importance = "permutation", 
  seed = 625,
  num.threads = 4
)

best_rf_predictions <- predict(best_rf_model, data = test_data)$predictions
best_rf_r2 <- calculate_r2(test_data$CostToRevenueRatio, best_rf_predictions)
best_rf_rmse <- sqrt(mean((test_data$CostToRevenueRatio - best_rf_predictions)^2))

variable_importance <- data.frame(
  Variable = names(best_rf_model$variable.importance),
  Importance = best_rf_model$variable.importance
)
variable_importance <- variable_importance[order(-variable_importance$Importance), ]  # Sort by importance
best_rf_r2


print("Top 10 Influential Variables:")
print(head(variable_importance, 10))

# Create a bar plot for the top 10 influential variables
top_10_variables <- head(variable_importance, 10)

plot <- ggplot(top_10_variables, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip coordinates to make it horizontal
  labs(
    title = "Top 10 Influential Variables",
    x = "Variable",
    y = "Importance"
  ) +
  theme_minimal()  # Use a clean theme

ggsave("./figures/top_10_influential_variables.png", plot = plot, width = 8, height = 6, dpi = 150)
