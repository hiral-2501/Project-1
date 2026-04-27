# Load required libraries
library(caret)
library(randomForest)
library(e1071)
library(xgboost)
library(class)
library(rpart)

# -------------------- Step 1: Load Pre-processed Datasets --------------------
# Load the pre-processed train and test datasets (both raw and normalized)
train_raw <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train_split_raw.csv")
train_norm <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train_split_normalized.csv")
test_raw <- read.csv("D:/DS Lab/PIU/PIU PROJECT/test_split_raw.csv")
test_norm <- read.csv("D:/DS Lab/PIU/PIU PROJECT/test_split_normalized.csv")

# Check the first few rows of the datasets to ensure they're loaded correctly
head(train_raw)
head(train_norm)
head(test_raw)
head(test_norm)

# -------------------- Step 2: Separate Target and Features --------------------
# Define the target column
target_col <- "PCIAT.PCIAT_Total"

# Separate predictors and target variable for training and test datasets
x_train_raw <- train_raw %>% select(-PCIAT.PCIAT_Total)
y_train_raw <- train_raw$PCIAT.PCIAT_Total

x_train_norm <- train_norm %>% select(-PCIAT.PCIAT_Total)
y_train_norm <- train_norm$PCIAT.PCIAT_Total

x_test_raw <- test_raw %>% select(-PCIAT.PCIAT_Total)
y_test_raw <- test_raw$PCIAT.PCIAT_Total

x_test_norm <- test_norm %>% select(-PCIAT.PCIAT_Total)
y_test_norm <- test_norm$PCIAT.PCIAT_Total

# -------------------- Step 3: Train the Models --------------------

# ------------------ 1. Linear Regression (using normalized data) ------------------
lm_model_norm <- lm(PCIAT.PCIAT_Total ~ ., data = train_norm)
lm_pred_norm <- predict(lm_model_norm, newdata = test_norm)

# ------------------ 2. Random Forest (using raw data) ------------------
set.seed(123)
rf_model_raw <- randomForest(x = x_train_raw, y = y_train_raw, ntree = 100, importance = TRUE)
rf_pred_raw <- predict(rf_model_raw, newdata = x_test_raw)

# ------------------ 3. Support Vector Machine (using normalized data) ------------------
set.seed(123)
svm_model_norm <- svm(x = x_train_norm, y = y_train_norm, type = "eps-regression", kernel = "radial")
svm_pred_norm <- predict(svm_model_norm, newdata = x_test_norm)

# ------------------ 4. K-Nearest Neighbors (using normalized data) ------------------
# KNN requires numerical features and target variable; also requires proper scaling
knn_model_norm <- knn(train = x_train_norm, test = x_test_norm, cl = y_train_norm, k = 5)
knn_pred_norm <- as.numeric(knn_model_norm)  # Convert to numeric

# ------------------ 5. XGBoost (using raw data) ------------------
# Convert to xgb.DMatrix format for raw data
xgb_train_raw <- xgb.DMatrix(data = as.matrix(x_train_raw), label = y_train_raw)
xgb_model_raw <- xgboost(data = xgb_train_raw, objective = "reg:squarederror", nrounds = 100, verbose = 0)
xgb_pred_raw <- predict(xgb_model_raw, newdata = as.matrix(x_test_raw))

# ------------------ 6. Decision Tree (using raw data) ------------------
tree_model_raw <- rpart(PCIAT.PCIAT_Total ~ ., data = train_raw)
tree_pred_raw <- predict(tree_model_raw, newdata = test_raw)

# -------------------- Step 4: Store the Predictions --------------------
# Create a data frame to store the predictions for each model
predictions <- data.frame(
  Actual = y_test_raw,
  lm_pred_norm = lm_pred_norm,
  rf_pred_raw = rf_pred_raw,
  svm_pred_norm = svm_pred_norm,
  knn_pred_norm = knn_pred_norm,
  xgb_pred_raw = xgb_pred_raw,
  tree_pred_raw = tree_pred_raw
)

# View the first few predictions
head(predictions)

# -------------------- Step 5: Evaluate the Models (Optional) --------------------
# Evaluate the models (using RMSE for regression)
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Evaluate models
lm_rmse <- rmse(y_test_norm, lm_pred_norm)
rf_rmse <- rmse(y_test_raw, rf_pred_raw)
svm_rmse <- rmse(y_test_norm, svm_pred_norm)
knn_rmse <- rmse(y_test_norm, knn_pred_norm)
xgb_rmse <- rmse(y_test_raw, xgb_pred_raw)
tree_rmse <- rmse(y_test_raw, tree_pred_raw)

# Print RMSE for each model
cat("RMSE for Linear Regression (Normalized):", lm_rmse, "\n")
cat("RMSE for Random Forest (Raw):", rf_rmse, "\n")
cat("RMSE for SVM (Normalized):", svm_rmse, "\n")
cat("RMSE for KNN (Normalized):", knn_rmse, "\n")
cat("RMSE for XGBoost (Raw):", xgb_rmse, "\n")
cat("RMSE for Decision Tree (Raw):", tree_rmse, "\n")
