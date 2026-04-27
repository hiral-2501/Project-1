
#################Training#######################################################


# Load required libraries
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(class)

# Set file paths
train_norm_path <- "D:/DS Lab/PIU/PIU PROJECT/train_split_normalized_filtered.csv"
test_norm_path  <- "D:/DS Lab/PIU/PIU PROJECT/test_split_normalized_filtered.csv"
train_raw_path  <- "D:/DS Lab/PIU/PIU PROJECT/train_split_raw_filtered.csv"
test_raw_path   <- "D:/DS Lab/PIU/PIU PROJECT/test_split_raw_filtered.csv"

# Load the datasets
train_norm <- read.csv(train_norm_path)
test_norm <- read.csv(test_norm_path)
train_raw <- read.csv(train_raw_path)
test_raw <- read.csv(test_raw_path)

# Separate predictors and target
target_col <- "PCIAT.PCIAT_Total"

# NORMALIZED DATA MODELS
x_train_norm <- train_norm[, !(names(train_norm) %in% target_col)]
y_train_norm <- train_norm[[target_col]]
x_test_norm  <- test_norm[, !(names(test_norm) %in% target_col)]
y_test_norm  <- test_norm[[target_col]]

# RAW DATA MODELS
x_train_raw <- train_raw[, !(names(train_raw) %in% target_col)]
y_train_raw <- train_raw[[target_col]]
x_test_raw  <- test_raw[, !(names(test_raw) %in% target_col)]
y_test_raw  <- test_raw[[target_col]]

# 1. Linear Regression (Normalized)
lm_model <- lm(PCIAT.PCIAT_Total ~ ., data = train_norm)
lm_pred <- predict(lm_model, newdata = test_norm)

# 2. SVM (Normalized)
svm_model <- svm(x_train_norm, y_train_norm)
svm_pred <- predict(svm_model, x_test_norm)

# 3. KNN (Normalized) - k = 5
knn_pred <- knn(train = x_train_norm, test = x_test_norm, cl = y_train_norm, k = 5)

# 4. Random Forest (Raw)
rf_model <- randomForest(x = x_train_raw, y = y_train_raw)
rf_pred <- predict(rf_model, newdata = x_test_raw)

# 5. XGBoost (Raw)
# Convert to matrix and DMatrix for XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(x_train_raw), label = y_train_raw)
dtest <- xgb.DMatrix(data = as.matrix(x_test_raw))

xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror", verbose = 0)
xgb_pred <- predict(xgb_model, newdata = dtest)

cat("✅ All 5 models have been trained and predictions made!\n\nNow you’re ready for evaluation.\n")





#############Evalutaion###############################################################################





install.packages("Metrics")

# Load required package for evaluation
library(Metrics)

# Create a function to evaluate predictions
evaluate_model <- function(true, pred, model_name) {
  rmse_val <- rmse(true, pred)
  mae_val <- mae(true, pred)
  r2_val <- 1 - sum((true - pred)^2) / sum((true - mean(true))^2)
  
  return(data.frame(
    Model = model_name,
    RMSE = round(rmse_val, 3),
    MAE = round(mae_val, 3),
    R_squared = round(r2_val, 3)
  ))
}

# Collect evaluation results
results <- rbind(
  evaluate_model(y_test_norm, lm_pred, "Linear Regression"),
  evaluate_model(y_test_norm, svm_pred, "SVM"),
  evaluate_model(y_test_norm, as.numeric(as.character(knn_pred)), "KNN"),
  evaluate_model(y_test_raw, rf_pred, "Random Forest"),
  evaluate_model(y_test_raw, xgb_pred, "XGBoost")
)

# Print results
print(results)



#######################################Plot to compare pred vs actual of all models######################

library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

# List of model predictions and names
predictions_list <- list(
  Linear_Regression = lm_pred,
  SVM = svm_pred,
  KNN = knn_pred,
  Random_Forest = rf_pred,
  XGBoost = xgb_pred
)

# Actual values from the test set (same for all)
actual_values <- test_split_raw$PCIAT.PCIAT_Total

# Generate ggplot objects for each model
plot_list <- lapply(names(predictions_list), function(model_name) {
  pred <- predictions_list[[model_name]]
  df <- data.frame(Actual = actual_values, Predicted = pred)
  
  ggplot(df, aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.6, color = "#1f77b4") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Actual vs Predicted -", model_name),
         x = "Actual PCIAT Score",
         y = "Predicted PCIAT Score") +
    theme_minimal()
})

# Arrange all plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

# Load required libraries
library(ggplot2)
library(dplyr)

# Assuming your model results are in this data frame
model_results <- data.frame(
  Model = c("Linear Regression", "SVM", "KNN", "Random Forest", "XGBoost"),
  RMSE = c(2.901, 2.808, 3.762, 2.861, 3.079),
  MAE = c(2.225, 2.123, 2.158, 2.222, 2.356),
  R_squared = c(0.967, 0.969, 0.945, 0.968, 0.963)
)

# Plot RMSE
rmse_plot <- ggplot(model_results, aes(x = reorder(Model, -RMSE), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison: RMSE", x = "Model", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot MAE
mae_plot <- ggplot(model_results, aes(x = reorder(Model, -MAE), y = MAE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison: MAE", x = "Model", y = "MAE") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot R-squared
r2_plot <- ggplot(model_results, aes(x = reorder(Model, R_squared), y = R_squared, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison: R-squared", x = "Model", y = "R-squared") +
  theme_minimal() +
  theme(legend.position = "none")

# Display plots in a grid
library(gridExtra)
grid.arrange(rmse_plot, mae_plot, r2_plot, nrow = 1)


saveRDS(svm_model, file = "D:/DS Lab/PIU/PIU PROJECT/final_svm_model.rds")








