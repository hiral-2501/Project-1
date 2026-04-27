# Load libraries
library(caret)

# Load the saved model
final_model <- readRDS("D:/DS Lab/PIU/PIU PROJECT/random_forest_model.rds")

# Create a function to take user input
get_user_input <- function() {
  cat("\nPlease enter the following values:\n")
  
  input_data <- data.frame(
    Basic_Demos.Age = as.numeric(readline("Age: ")),
    PreInt_EduHx.computerinternet_hoursday = as.numeric(readline("Daily Internet Usage (hours): ")),
    SDS.SDS_Total_T = as.numeric(readline("SDS Total T Score: ")),
    PCIAT.PCIAT_01 = as.numeric(readline("PCIAT Question 1: ")),
    PCIAT.PCIAT_02 = as.numeric(readline("PCIAT Question 2: ")),
    PCIAT.PCIAT_03 = as.numeric(readline("PCIAT Question 3: ")),
    PCIAT.PCIAT_04 = as.numeric(readline("PCIAT Question 4: ")),
    PCIAT.PCIAT_05 = as.numeric(readline("PCIAT Question 5: ")),
    PCIAT.PCIAT_06 = as.numeric(readline("PCIAT Question 6: ")),
    PCIAT.PCIAT_07 = as.numeric(readline("PCIAT Question 7: ")),
    PCIAT.PCIAT_08 = as.numeric(readline("PCIAT Question 8: ")),
    PCIAT.PCIAT_09 = as.numeric(readline("PCIAT Question 9: ")),
    PCIAT.PCIAT_10 = as.numeric(readline("PCIAT Question 10: ")),
    PCIAT.PCIAT_11 = as.numeric(readline("PCIAT Question 11: ")),
    PCIAT.PCIAT_12 = as.numeric(readline("PCIAT Question 12: ")),
    PCIAT.PCIAT_13 = as.numeric(readline("PCIAT Question 13: ")),
    PCIAT.PCIAT_14 = as.numeric(readline("PCIAT Question 14: ")),
    PCIAT.PCIAT_15 = as.numeric(readline("PCIAT Question 15: ")),
    PCIAT.PCIAT_16 = as.numeric(readline("PCIAT Question 16: ")),
    PCIAT.PCIAT_17 = as.numeric(readline("PCIAT Question 17: ")),
    PCIAT.PCIAT_18 = as.numeric(readline("PCIAT Question 18: ")),
    PCIAT.PCIAT_19 = as.numeric(readline("PCIAT Question 19: ")),
    PCIAT.PCIAT_20 = as.numeric(readline("PCIAT Question 20: "))
  )
  
  return(input_data)
}

# Loop to allow multiple predictions

repeat {
  user_input <- get_user_input()
  
  # Predict using the loaded model
  prediction <- predict(final_model, newdata = user_input)
  
  cat("\n🔮 Predicted PCIAT Total Score:", round(prediction, 2), "\n")
  
  # Ask if user wants to predict again
  continue <- readline("\nDo you want to make another prediction? (yes/no): ")
  if (tolower(continue) != "yes") {
    cat("Exiting prediction loop. ✅\n")
    break
  }
}

