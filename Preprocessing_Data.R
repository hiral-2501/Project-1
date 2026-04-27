selected_features <- c(
  "Basic_Demos-Age",
  "PreInt_EduHx-computerinternet_hoursday",
  "SDS-SDS_Total_Ts",
  
  # PCIAT items
  "PCIAT-PCIAT_01", "PCIAT-PCIAT_02", "PCIAT-PCIAT_03", "PCIAT-PCIAT_04",
  "PCIAT-PCIAT_05", "PCIAT-PCIAT_06", "PCIAT-PCIAT_07", "PCIAT-PCIAT_08",
  "PCIAT-PCIAT_09", "PCIAT-PCIAT_10", "PCIAT-PCIAT_11", "PCIAT-PCIAT_12",
  "PCIAT-PCIAT_13", "PCIAT-PCIAT_14", "PCIAT-PCIAT_15", "PCIAT-PCIAT_16",
  "PCIAT-PCIAT_17", "PCIAT-PCIAT_18", "PCIAT-PCIAT_19", "PCIAT-PCIAT_20",
  
  # PCIAT Total Score (Target variable)
  "PCIAT-PCIAT_Total"
)
library(dplyr)

# Load original datasets
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")

colnames(train_data)

# Select the exact 25 features from both train and test
train_selected <- train_data %>% select(all_of(selected_features))
test_selected <- test_data %>% select(all_of(selected_features))

# Save to new CSV files
write.csv(train_selected, "train_selected_25features.csv", row.names = FALSE)
write.csv(test_selected, "test_selected_25features.csv", row.names = FALSE)