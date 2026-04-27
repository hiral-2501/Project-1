# Load full train.csv
train <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train.csv")

# List of 24 required columns (23 predictors + 1 target)
selected_features <- c(
  "Basic_Demos.Age",
  "PreInt_EduHx.computerinternet_hoursday",
  "SDS.SDS_Total_T",
  paste0("PCIAT.PCIAT_", sprintf("%02d", 1:20)),
  "PCIAT.PCIAT_Total"
)

# Subset and create new dataset
train_24 <- train[, selected_features]

# Save to CSV
write.csv(train_24, "D:/DS Lab/PIU/PIU PROJECT/new_data_24.csv", row.names = FALSE)

cat("✅ Saved: new_data_24.csv with selected 24 features.\n")
