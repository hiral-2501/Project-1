# Load required library
library(caret)

# Read the cleaned and split dataset
train_split <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train_split.csv")

# Separate target and predictors
target_col <- "PCIAT.PCIAT_Total"
predictors <- setdiff(names(train_split), target_col)

# 1. Create a normalized version of the dataset (Min-Max Scaling)
preprocess_norm <- preProcess(train_split[, predictors], method = c("range"))
train_normalized <- predict(preprocess_norm, train_split[, predictors])
train_normalized[[target_col]] <- train_split[[target_col]]  # Add back target

# 2. Keep the original version (no scaling)
train_raw <- train_split

# Optional: write both versions to CSV
write.csv(train_normalized, "D:/DS Lab/PIU/PIU PROJECT/train_split_normalized.csv", row.names = FALSE)
write.csv(train_raw,        "D:/DS Lab/PIU/PIU PROJECT/train_split_raw.csv", row.names = FALSE)

# Read the test set
test_split <- read.csv("D:/DS Lab/PIU/PIU PROJECT/test_split.csv")

# Separate target and predictors
target_col <- "PCIAT.PCIAT_Total"
predictors <- setdiff(names(test_split), target_col)

# Use the same preprocessing object created from training data
# (make sure 'preprocess_norm' is from the previous step)
test_normalized <- predict(preprocess_norm, test_split[, predictors])
test_normalized[[target_col]] <- test_split[[target_col]]  # Add back target

# Original test data (no scaling)
test_raw <- test_split

# Save both versions to CSV
write.csv(test_normalized, "D:/DS Lab/PIU/PIU PROJECT/test_split_normalized.csv", row.names = FALSE)
write.csv(test_raw,        "D:/DS Lab/PIU/PIU PROJECT/test_split_raw.csv", row.names = FALSE)


###########==========================================================###########

# Load the datasets
train_split_raw <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train_split_raw.csv")
train_split_normalized <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train_split_normalized.csv")
test_split_raw <- read.csv("D:/DS Lab/PIU/PIU PROJECT/test_split_raw.csv")
test_split_normalized <- read.csv("D:/DS Lab/PIU/PIU PROJECT/test_split_normalized.csv")

# Selected features as provided
selected_features <- c(
  "Basic_Demos.Age",
  "PreInt_EduHx.computerinternet_hoursday",
  "SDS.SDS_Total_T",
  "PCIAT.PCIAT_01", "PCIAT.PCIAT_02", "PCIAT.PCIAT_03", "PCIAT.PCIAT_04", "PCIAT.PCIAT_05",
  "PCIAT.PCIAT_06", "PCIAT.PCIAT_07", "PCIAT.PCIAT_08", "PCIAT.PCIAT_09", "PCIAT.PCIAT_10",
  "PCIAT.PCIAT_11", "PCIAT.PCIAT_12", "PCIAT.PCIAT_13", "PCIAT.PCIAT_14", "PCIAT.PCIAT_15",
  "PCIAT.PCIAT_16", "PCIAT.PCIAT_17", "PCIAT.PCIAT_18", "PCIAT.PCIAT_19", "PCIAT.PCIAT_20",
  "PCIAT.PCIAT_Total"
)

# Function to filter datasets based on selected features
filter_dataset <- function(data, selected_features) {
  data_filtered <- data[, selected_features]
  return(data_filtered)
}

# Filter all the datasets
train_split_raw_filtered <- filter_dataset(train_split_raw, selected_features)
train_split_normalized_filtered <- filter_dataset(train_split_normalized, selected_features)
test_split_raw_filtered <- filter_dataset(test_split_raw, selected_features)
test_split_normalized_filtered <- filter_dataset(test_split_normalized, selected_features)

# Save the filtered datasets as new CSV files
write.csv(train_split_raw_filtered, "D:/DS Lab/PIU/PIU PROJECT/train_split_raw_filtered.csv", row.names = FALSE)
write.csv(train_split_normalized_filtered, "D:/DS Lab/PIU/PIU PROJECT/train_split_normalized_filtered.csv", row.names = FALSE)
write.csv(test_split_raw_filtered, "D:/DS Lab/PIU/PIU PROJECT/test_split_raw_filtered.csv", row.names = FALSE)
write.csv(test_split_normalized_filtered, "D:/DS Lab/PIU/PIU PROJECT/test_split_normalized_filtered.csv", row.names = FALSE)

# Confirmation message
cat("Filtered datasets saved successfully!\n")






