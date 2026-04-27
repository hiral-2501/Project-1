# Load caret and ggplot2
library(caret)
library(ggplot2)

# Load the train and test datasets
train_data <- read.csv("D:/DS Lab/PIU/PIU PROJECT/new_train_data_24.csv")
test_data <- read.csv("D:/DS Lab/PIU/PIU PROJECT/new_test_data_24.csv")

# Split predictors and target
x_train <- train_data[, -which(names(train_data) == "PCIAT.PCIAT_Total")]
y_train <- train_data$PCIAT.PCIAT_Total

x_test <- test_data[, -which(names(test_data) == "PCIAT.PCIAT_Total")]
y_test <- test_data$PCIAT.PCIAT_Total

# Define training control
train_control <- trainControl(
  method = "cv", 
  number = 5,
  summaryFunction = defaultSummary,  
  verboseIter = TRUE  
)

# Train the SVM model
set.seed(123)
svm_model <- train(
  x = x_train,
  y = y_train,
  method = "svmLinear",
  preProcess = c("center", "scale"),
  trControl = train_control,
  metric = "RMSE"
)

# See cross-validation results
print(svm_model)

# Predict on unseen test data
predictions <- predict(svm_model, newdata = x_test)

# Evaluate performance on unseen test data
test_results <- postResample(pred = predictions, obs = y_test)

cat("\n\n📢 Final Performance on Unseen Test Data:\n")
print(test_results)

# --------- ✨ Plot Actual vs Predicted ---------

# Create a dataframe for plotting
plot_data <- data.frame(
  Actual = y_test,
  Predicted = predictions
)

# Create the plot
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +  # scatter points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # ideal line
  labs(
    title = "Actual vs Predicted PCIAT Scores",
    x = "Actual PCIAT.PCIAT_Total",
    y = "Predicted PCIAT.PCIAT_Total"
  ) +
  theme_minimal()

























train_data <- read.csv("new_train_data_24.csv")
test_data <- read.csv("new_test_data_24.csv")
# Load libraries
library(caret)
library(xgboost)
library(randomForest)
library(ggplot2)

# Set cross-validation control
control <- trainControl(method = "cv", number = 5, savePredictions = "final")

# Train Linear Regression
set.seed(123)
lm_model <- train(PCIAT.PCIAT_Total ~ ., data = train_data, method = "lm", trControl = control, preProcess = c("center", "scale"))

# Train KNN Regression
set.seed(123)
knn_model <- train(PCIAT.PCIAT_Total ~ ., data = train_data, method = "knn", trControl = control, preProcess = c("center", "scale"), tuneLength = 5)

# Train Random Forest
set.seed(123)
rf_model <- train(PCIAT.PCIAT_Total ~ ., data = train_data, method = "rf", trControl = control, tuneLength = 5)

# Train XGBoost
set.seed(123)
xgb_model <- train(PCIAT.PCIAT_Total ~ ., data = train_data, method = "xgbLinear", trControl = control, tuneLength = 5)

# Evaluate the models using the test data
lm_predictions <- predict(lm_model, newdata = test_data)
knn_predictions <- predict(knn_model, newdata = test_data)
rf_predictions <- predict(rf_model, newdata = test_data)
xgb_predictions <- predict(xgb_model, newdata = test_data)

# Combine predictions with actual values
predicted_results <- data.frame(
  Actual = test_data$PCIAT.PCIAT_Total,
  LM = lm_predictions,
  KNN = knn_predictions,
  RF = rf_predictions,
  XGBoost = xgb_predictions
)

# Evaluate RMSE, MAE, R² on Test Data
evaluate_metrics <- function(predictions, actuals) {
  rmse <- sqrt(mean((predictions - actuals)^2))
  mae <- mean(abs(predictions - actuals))
  r2 <- 1 - (sum((predictions - actuals)^2) / sum((actuals - mean(actuals))^2))
  return(c(RMSE = rmse, MAE = mae, R2 = r2))
}

# Compare performance on Test Data
model_metrics <- data.frame(
  Model = c("Linear Regression", "KNN", "Random Forest", "XGBoost"),
  t(evaluate_metrics(lm_predictions, test_data$PCIAT.PCIAT_Total)),
  t(evaluate_metrics(knn_predictions, test_data$PCIAT.PCIAT_Total)),
  t(evaluate_metrics(rf_predictions, test_data$PCIAT.PCIAT_Total)),
  t(evaluate_metrics(xgb_predictions, test_data$PCIAT.PCIAT_Total))
)

# Print evaluation summary
print(model_metrics)

# Cross-validation results
cv_results <- resamples(list(
  LinearRegression = lm_model,
  KNN = knn_model,
  RandomForest = rf_model,
  XGBoost = xgb_model
))

# Summary of Cross-validation
summary(cv_results)

# Plot comparison of predicted vs actual values for each model
ggplot(predicted_results, aes(x = Actual)) +
  geom_point(aes(y = LM, color = "Linear Regression")) +
  geom_point(aes(y = KNN, color = "KNN")) +
  geom_point(aes(y = RF, color = "Random Forest")) +
  geom_point(aes(y = XGBoost, color = "XGBoost")) +
  geom_smooth(aes(y = Actual), color = "black", linetype = "dashed") +
  labs(title = "Predicted vs Actual", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Plot Boxplot of Cross-validation Results
bwplot(cv_results)


saveRDS(rf_model, "D:/DS Lab/PIU/PIU PROJECT/random_forest_model.rds")
loaded_rf_model <- readRDS("D:/DS Lab/PIU/PIU PROJECT/random_forest_model.rds")

# Print a quick summary
print(loaded_rf_model)



