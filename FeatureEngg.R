library(dplyr)
library(caret)

# Step 1: Load the dataset
train_data <- read.csv("train.csv")

# Step 2: Define 24 selected features (excluding BMI)
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

# Step 3: Subset the data
data_subset <- train_data %>%
  select(all_of(selected_features))

# Step 4: Convert all columns to numeric
data_subset <- data_subset %>%
  mutate(across(everything(), as.numeric))

# Step 5: Handle missing values
# Print missing value percentage
missing_pct <- sapply(data_subset, function(x) sum(is.na(x)) / length(x) * 100)
print("Missing Value Percentages:")
print(missing_pct)

# Impute numeric columns using median
data_imputed <- data_subset %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Step 6: Optional - Outlier detection (IQR method) - just mark for now

detect_outliers_iqr <- function(df) {
  outlier_summary <- data.frame(Feature = character(), Outlier_Count = integer(), Outlier_Percentage = numeric())
  
  for (col in names(df)) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    outlier_count <- sum(df[[col]] < lower_bound | df[[col]] > upper_bound)
    outlier_pct <- (outlier_count / nrow(df)) * 100
    
    outlier_summary <- rbind(outlier_summary, data.frame(
      Feature = col,
      Outlier_Count = outlier_count,
      Outlier_Percentage = round(outlier_pct, 2)
    ))
  }
  return(outlier_summary)
}

outlier_report <- detect_outliers_iqr(data_imputed)
print(outlier_report[order(-outlier_report$Outlier_Percentage), ])


#winorisation 
winsorize <- function(x, lower_quant = 0.05, upper_quant = 0.95) {
  qnt <- quantile(x, probs = c(lower_quant, upper_quant), na.rm = TRUE)
  x[x < qnt[1]] <- qnt[1]
  x[x > qnt[2]] <- qnt[2]
  return(x)
}

# Apply Winsorization to the columns with >20% outliers
cols_high_outliers <- c("PCIAT.PCIAT_01", "PCIAT.PCIAT_05", "PCIAT.PCIAT_07")

# Winsorize the columns and merge back to the dataset
data_step1 <- data_imputed  # Create a copy of the dataset
data_step1[cols_high_outliers] <- lapply(data_step1[cols_high_outliers], winsorize)


# Recalculate the outlier count and percentage after Winsorization for the specified columns
outlier_report_after_winsorization <- sapply(data_step1[cols_high_outliers], function(x) {
  lower_bound <- quantile(x, 0.05, na.rm = TRUE)
  upper_bound <- quantile(x, 0.95, na.rm = TRUE)
  
  # Count outliers based on the new limits
  outlier_count <- sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
  outlier_percentage <- (outlier_count / length(x)) * 100
  
  # Return a named vector with the counts and percentages
  return(c(Outlier_Count = outlier_count, Outlier_Percentage = outlier_percentage))
})

# Convert the result to a data frame for better readability
outlier_report_after_winsorization_df <- as.data.frame(t(outlier_report_after_winsorization))

# Print the recalculated outlier report
print(outlier_report_after_winsorization_df)

# Function to perform Winsorization (capping at 5th and 95th percentiles)
winsorize <- function(x) {
  lower_limit <- quantile(x, 0.05, na.rm = TRUE)
  upper_limit <- quantile(x, 0.95, na.rm = TRUE)
  x[x < lower_limit] <- lower_limit
  x[x > upper_limit] <- upper_limit
  return(x)
}

# List of columns to perform Winsorization on
columns_to_win <- c('PCIAT.PCIAT_03', 'PCIAT.PCIAT_Total', 'PCIAT.PCIAT_12', 
                    'PCIAT.PCIAT_08', 'PCIAT.PCIAT_11', 'SDS.SDS_Total_T', 
                    'PCIAT.PCIAT_18', 'PCIAT.PCIAT_19')

# Ensure all columns are numeric before Winsorization
train_data[columns_to_win] <- lapply(train_data[columns_to_win], function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(as.character(x))  # Convert to numeric if not already
  }
  return(x)
})

# Check the data types before applying Winsorization
print(sapply(train_data[columns_to_win], class))  # This will print the class of each column

# Apply Winsorization on the selected columns
train_data[columns_to_win] <- lapply(train_data[columns_to_win], winsorize)

# Check the data types after applying Winsorization
print(sapply(train_data[columns_to_win], class))  # Ensure columns are numeric

# Function to detect outliers after Winsorization
detect_outliers <- function(data, columns) {
  outlier_report <- data.frame(Feature = character(), Outlier_Count = numeric(), Outlier_Percentage = numeric())
  
  for (col in columns) {
    lower_limit <- quantile(data[[col]], 0.05, na.rm = TRUE)
    upper_limit <- quantile(data[[col]], 0.95, na.rm = TRUE)
    outliers <- sum(data[[col]] < lower_limit | data[[col]] > upper_limit, na.rm = TRUE)
    outlier_percentage <- (outliers / nrow(data)) * 100
    outlier_report <- rbind(outlier_report, data.frame(Feature = col, Outlier_Count = outliers, Outlier_Percentage = outlier_percentage))
  }
  
  outlier_report <- outlier_report[order(-outlier_report$Outlier_Percentage), ]
  return(outlier_report)
}

# Detect outliers after Winsorization
outlier_report_after_winsorization <- detect_outliers(train_data, columns_to_win)
print(outlier_report_after_winsorization)


train_data$PCIAT.PCIAT_Total[is.na(train_data$PCIAT.PCIAT_Total)] <- 
  median(train_data$PCIAT.PCIAT_Total, na.rm = TRUE)

# Now split into 80% training and 20% testing
library(caret)
set.seed(123)  # For reproducibility

train_index <- createDataPartition(train_data$PCIAT.PCIAT_Total, p = 0.8, list = FALSE)
train_split <- train_data[train_index, ]
test_split <- train_data[-train_index, ]

# Optionally save to CSV
write.csv(train_split, "D:/DS Lab/PIU/PIU PROJECT/train_split.csv", row.names = FALSE)
write.csv(test_split, "D:/DS Lab/PIU/PIU PROJECT/test_split.csv", row.names = FALSE)
