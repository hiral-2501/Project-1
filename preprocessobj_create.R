library(caret)

# Read the 24-feature training dataset
train_selected <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train_split_normalized_filtered.csv")

# Define target and predictors
target_col <- "PCIAT.PCIAT_Total"
predictors <- setdiff(names(train_selected), target_col)

# Create Min-Max normalization preprocessing object
preprocess_norm <- preProcess(train_selected[, predictors], method = c("range"))

# Save it for deployment use
saveRDS(preprocess_norm, "D:/DS Lab/PIU/PIU PROJECT/preprocess_norm.rds")
