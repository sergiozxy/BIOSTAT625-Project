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



results_svm_linear = c()
results_svm_polynomial = c()
results_svm_radial = c()
results_knn_gaussian = c()
results_knn_rec = c()
results_knn_tri = c()

#5-Fold Cross-Validation
set.seed(625)

for (i in 1:5) {
  
  train_data = cleaned_data_scale[folds[[i]], ]
  test_data = cleaned_data_scale[-folds[[i]], ]
  
  
  knn_model = kknn( CostToRevenueRatio ~ ., train_data, test_data, k = 5, kernel = "gaussian")
  
  predictions = fitted(knn_model)
  rmse = sqrt(mean((predictions - test_data$CostToRevenueRatio)^2))  # RMSE 作为指标
  results_knn_gaussian = c(results_knn_gaussian, rmse)
}

mean(results_svm_linear)
mean(results_svm_polynomial)
mean(results_svm_radial)

mean(results_knn_rec)
mean(results_knn_tri)
mean(results_knn_gaussian)


set.seed(625)

train_indices = sample(1:nrow(cleaned_data_scale), 0.8 * nrow(cleaned_data_scale))  # 80% 训练集
train_data = cleaned_data_scale[train_indices, ]
test_data = cleaned_data_scale[-train_indices, ]

svm_model = svm(CostToRevenueRatio ~ ., data = train_data, type = "eps-regression", kernel = "linear")
svm_model = svm(CostToRevenueRatio ~ ., data = train_data, type = "eps-regression", kernel = "polynomial")
svm_model = svm(CostToRevenueRatio ~ ., data = train_data, type = "eps-regression", kernel = "radial")

predicted_values = predict(svm_model,test_data)
mse = mean((predicted_values - test_data$CostToRevenueRatio)^2)
rmse = sqrt(mse)

knn_model = kknn( CostToRevenueRatio ~ ., train_data, test_data, k = 5, kernel = "rectangular")
knn_model = kknn( CostToRevenueRatio ~ ., train_data, test_data, k = 5, kernel = "gaussian")
knn_model = kknn( CostToRevenueRatio ~ ., train_data, test_data, k = 5, kernel = "triangular")
knn_model = kknn( CostToRevenueRatio ~ ., train_data, test_data, k = 5, kernel = "optimal")
predicted_price = fitted(knn_model)
mse = mean((predicted_price - test_data$CostToRevenueRatio)^2)
rmse = sqrt(mse)