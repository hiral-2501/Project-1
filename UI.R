library(shiny)

# -------------------------- UI --------------------------
ui <- fluidPage(
  titlePanel("Problematic Internet Use (PIU) Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "1. What is your age?", value = 18, min = 7, max = 25),
      numericInput("internet_hours", "2. How many hours per day do you use the computer/internet?", value = 3, min = 0, max = 24),
      numericInput("sds_score", "3. What is your SDS (Self-Depression Score)?", value = 50, min = 0, max = 100),
      h4("Please rate the following PCIAT (Internet Addiction) questions from 1 (Rarely) to 5 (Always):"),
      lapply(1:20, function(i) {
        numericInput(paste0("pciat_", sprintf("%02d", i)),
                     paste(i + 3, paste("PCIAT", i, "response:"), sep = ". "),
                     value = 3, min = 1, max = 5)
      }),
      actionButton("submit", "Predict PIU Score")
    ),
    mainPanel(
      h3("Predicted PCIAT Total Score:"),
      verbatimTextOutput("prediction")
    )
  )
)

# ------------------------ SERVER ------------------------
server <- function(input, output) {
  
  # Load the trained caret SVM model and the training dataset
  svm_model <- readRDS("D:/DS Lab/PIU/PIU PROJECT/final_svm_model.rds")
  train_data <- read.csv("D:/DS Lab/PIU/PIU PROJECT/train_split_raw_filtered.csv")
  
  observeEvent(input$submit, {
    
    # 1. Collect user input
    user_input <- data.frame(
      Basic_Demos.Age = input$age,
      PreInt_EduHx.computerinternet_hoursday = input$internet_hours,
      SDS.SDS_Total_T = input$sds_score,
      PCIAT.PCIAT_01 = input$pciat_01,
      PCIAT.PCIAT_02 = input$pciat_02,
      PCIAT.PCIAT_03 = input$pciat_03,
      PCIAT.PCIAT_04 = input$pciat_04,
      PCIAT.PCIAT_05 = input$pciat_05,
      PCIAT.PCIAT_06 = input$pciat_06,
      PCIAT.PCIAT_07 = input$pciat_07,
      PCIAT.PCIAT_08 = input$pciat_08,
      PCIAT.PCIAT_09 = input$pciat_09,
      PCIAT.PCIAT_10 = input$pciat_10,
      PCIAT.PCIAT_11 = input$pciat_11,
      PCIAT.PCIAT_12 = input$pciat_12,
      PCIAT.PCIAT_13 = input$pciat_13,
      PCIAT.PCIAT_14 = input$pciat_14,
      PCIAT.PCIAT_15 = input$pciat_15,
      PCIAT.PCIAT_16 = input$pciat_16,
      PCIAT.PCIAT_17 = input$pciat_17,
      PCIAT.PCIAT_18 = input$pciat_18,
      PCIAT.PCIAT_19 = input$pciat_19,
      PCIAT.PCIAT_20 = input$pciat_20
    )
    
    # 2. Normalize using Min-Max Scaling from training data
    predictors <- names(user_input)
    min_vals <- sapply(train_data[, predictors], min)
    max_vals <- sapply(train_data[, predictors], max)
    
    user_scaled <- as.data.frame(mapply(
      function(x, min_val, max_val) {
        if (max_val == min_val) return(0)
        (x - min_val) / (max_val - min_val)
      },
      x = user_input,
      min_val = min_vals,
      max_val = max_vals
    ))
    
    # 3. Predict (IMPORTANT: remove newdata = )
    predicted_score <- predict(svm_model, user_scaled)
    
    # 4. Display output
    output$prediction <- renderText({
      paste("Predicted PCIAT Total Score:", round(predicted_score, 2))
    })
    
  })
  
}

# ------------------------ RUN APP ------------------------
shinyApp(ui = ui, server = server)




