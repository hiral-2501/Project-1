# Load required libraries
library(caret)   # For preprocessing
library(tidyverse)  # For data manipulation

# Read in the filtered dataset
train_raw_filtered <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train_split_raw_filtered.csv")
train_norm_filtered <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train_split_normalized_filtered.csv")
test_raw_filtered <- read.csv("D:/DS Lab/PIU/PIU PROJECT/test_split_raw_filtered.csv")
test_norm_filtered <- read.csv("D:/DS Lab/PIU/PIU PROJECT/test_split_normalized_filtered.csv")

# Step 1: Handle Missing Values - Imputation
impute_missing_values <- function(data) {
  pre_process <- preProcess(data, method = "medianImpute")  # Use median for imputation
  data_imputed <- predict(pre_process, data)
  return(data_imputed)
}

# Impute missing values for each dataset
train_raw_imputed <- impute_missing_values(train_raw_filtered)
train_norm_imputed <- impute_missing_values(train_norm_filtered)
test_raw_imputed <- impute_missing_values(test_raw_filtered)
test_norm_imputed <- impute_missing_values(test_norm_filtered)

# Save the imputed datasets back into the same files
write.csv(train_raw_imputed, "D:/DS Lab/PIU/PIU PROJECT/train_split_raw_filtered.csv", row.names = FALSE)
write.csv(train_norm_imputed, "D:/DS Lab/PIU/PIU PROJECT/train_split_normalized_filtered.csv", row.names = FALSE)
write.csv(test_raw_imputed, "D:/DS Lab/PIU/PIU PROJECT/test_split_raw_filtered.csv", row.names = FALSE)
write.csv(test_norm_imputed, "D:/DS Lab/PIU/PIU PROJECT/test_split_normalized_filtered.csv", row.names = FALSE)

cat("Missing values have been imputed and datasets have been saved successfully.\n")
