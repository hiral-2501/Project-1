# Load necessary libraries
library(dplyr)

# Load your sliced dataset (new_data_24.csv)
data <- read.csv("D:/DS Lab/PIU/PIU PROJECT/new_data_24.csv")

# View basic summary of the dataset
summary(data)

# Check missing values per column
colSums(is.na(data))

# Impute missing values with Median
# (Median is better than mean because it's more robust against outliers)

data_cleaned <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Double check missing values are handled
colSums(is.na(data_cleaned))

# Save the cleaned data for further steps
write.csv(data_cleaned, "D:/DS Lab/PIU/PIU PROJECT/new_data_24_cleaned.csv", row.names = FALSE)

cat("✅ Data cleaning completed and saved as 'new_data_24_cleaned.csv'\n")




































# Load necessary library

library(dplyr)

# Load the cleaned dataset
data_cleaned <- read.csv("D:/DS Lab/PIU/PIU PROJECT/new_data_24_cleaned.csv")

# Define predictor columns (exclude target)
predictor_cols <- names(data_cleaned)[names(data_cleaned) != "PCIAT.PCIAT_Total"]

# --------------------------
# Step 1: Outlier Checking (1%-99%)
# --------------------------
outlier_percentage <- function(x) {
  qnt <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  low_outliers <- sum(x < qnt[1], na.rm = TRUE)
  high_outliers <- sum(x > qnt[2], na.rm = TRUE)
  total <- length(x)
  perc <- (low_outliers + high_outliers) / total * 100
  return(round(perc, 2))  # return percentage
}

# Apply outlier detection
outlier_summary_before <- sapply(data_cleaned[predictor_cols], outlier_percentage)

# Print outlier summary before
cat("📋 Outlier Summary BEFORE Winsorization (1%-99%):\n")
print(outlier_summary_before)


# --------------------------
# Step 2: Winsorization (1%-99%)
# --------------------------
# Define features where outliers are present
features_to_winsorize <- names(outlier_summary_before[outlier_summary_before > 0])

# Winsorization function
winsorize <- function(x) {
  qnt <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  x[x < qnt[1]] <- qnt[1]
  x[x > qnt[2]] <- qnt[2]
  return(x)
}

# Apply winsorization only on needed features
data_winsorized <- data_cleaned
data_winsorized[features_to_winsorize] <- lapply(data_cleaned[features_to_winsorize], winsorize)


# --------------------------
# Step 3: Re-check Outliers (1%-99%)
# --------------------------
# Same 1–99% method for rechecking
outlier_summary_after <- sapply(data_winsorized[predictor_cols], outlier_percentage)

cat("\n📋 Outlier Summary AFTER Winsorization (1%-99%):\n")
print(outlier_summary_after)


# --------------------------
# Step 4: Save the file
# --------------------------
write.csv(data_winsorized, "D:/DS Lab/PIU/PIU PROJECT/new_data_24_cleaned_no_outliers.csv", row.names = FALSE)

cat("\n✅ Winsorization complete and saved.\n")
























# Load necessary libraries
library(caret)

# Load the cleaned dataset (without outliers)
data_final <- read.csv("D:/DS Lab/PIU/PIU PROJECT/new_data_24_cleaned_no_outliers.csv")

# Set seed for reproducibility
set.seed(123)

# Create the train/test split index using caret's createDataPartition
trainIndex <- createDataPartition(data_final$PCIAT.PCIAT_Total, p = 0.8, list = FALSE)

# Split the data
train_data <- data_final[trainIndex, ]
test_data  <- data_final[-trainIndex, ]

# Save the split datasets
write.csv(train_data, "D:/DS Lab/PIU/PIU PROJECT/new_train_data_24.csv", row.names = FALSE)
write.csv(test_data, "D:/DS Lab/PIU/PIU PROJECT/new_test_data_24.csv", row.names = FALSE)












