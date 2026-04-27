# Load libraries
library(caret)

# Load the saved Random Forest model
final_model <- readRDS("D:/DS Lab/PIU/PIU PROJECT/random_forest_model.rds")

# Create a sample input (one row dataframe)
sample_input <- data.frame(
  Basic_Demos.Age = 21,
  PreInt_EduHx.computerinternet_hoursday = 5,
  SDS.SDS_Total_T = 45,
  PCIAT.PCIAT_01 = 3,
  PCIAT.PCIAT_02 = 2,
  PCIAT.PCIAT_03 = 0,
  PCIAT.PCIAT_04 = 3,
  PCIAT.PCIAT_05 = 2,
  PCIAT.PCIAT_06 = 5,
  PCIAT.PCIAT_07 = 1,
  PCIAT.PCIAT_08 = 4,
  PCIAT.PCIAT_09 = 3,
  PCIAT.PCIAT_10 = 2,
  PCIAT.PCIAT_11 = 5,
  PCIAT.PCIAT_12 = 4,
  PCIAT.PCIAT_13 = 3,
  PCIAT.PCIAT_14 = 2,
  PCIAT.PCIAT_15 = 3,
  PCIAT.PCIAT_16 = 4,
  PCIAT.PCIAT_17 = 3,
  PCIAT.PCIAT_18 = 2,
  PCIAT.PCIAT_19 = 3,
  PCIAT.PCIAT_20 = 4
)

# Make prediction
prediction <- predict(final_model, newdata = sample_input)

# Show prediction
print(paste("Predicted PCIAT Total Score:", round(prediction, 2)))
