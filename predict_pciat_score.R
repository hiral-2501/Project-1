# predict_pciat_score.R

library(caret)

# Load the trained SVM model and preprocessing object
svm_model <- readRDS("D:/DS Lab/PIU/PIU PROJECT/final_svm_model.rds")
preprocess_norm <- readRDS("D:/DS Lab/PIU/PIU PROJECT/preprocess_norm.rds")

# PCIAT Category function
classify_pciat <- function(score) {
  if (score >= 80) {
    return("Severe")
  } else if (score >= 50) {
    return("Moderate")
  } else {
    return("Mild")
  }
}

# Prediction function
predict_pciat_score <- function(input_df) {
  # Normalize the input using the saved preprocessing object
  input_norm <- predict(preprocess_norm, input_df)
  
  # Predict using the SVM model
  predicted_score <- predict(svm_model, newdata = input_norm)
  
  # Classify the score
  category <- classify_pciat(predicted_score)
  
  return(list(score = round(predicted_score, 2), category = category))
}
