library(shiny)
library(randomForest)

# Load the trained Random Forest model
rf_model <- readRDS("random_forest_model.rds") 

# Define the UI
ui <- fluidPage(
  titlePanel("Problematic Internet Use Prediction (PCIAT Total Score)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("Age", "Age:", value = 20, min = 5, max = 25),
      numericInput("InternetHours", "Computer/Internet Hours Per Day:", value = 4, min = 0, max = 24),
      numericInput("SDS_Total_T", "SDS Total T Score:", value = 50, min = 0, max = 100),
      
      numericInput("PCIAT_01", "Q1 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_02", "Q2 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_03", "Q3 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_04", "Q4 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_05", "Q5 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_06", "Q6 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_07", "Q7 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_08", "Q8 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_09", "Q9 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_10", "Q10 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_11", "Q11 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_12", "Q12 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_13", "Q13 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_14", "Q14 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_15", "Q15 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_16", "Q16 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_17", "Q17 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_18", "Q18 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_19", "Q19 Score:", value = 3, min = 0, max = 5),
      numericInput("PCIAT_20", "Q20 Score:", value = 3, min = 0, max = 5),
      
      actionButton("predict_btn", "Predict PCIAT Total")
    ),
    
    mainPanel(
      h3("Predicted PCIAT Total Score:"),
      textOutput("prediction_output")  # <- CHANGED from 'verbatimTextOutput' to 'textOutput'
    )
  )
)

# Define the Server
server <- function(input, output) {
  
  user_data <- reactive({
    req(input$predict_btn)
    
    data.frame(
      Basic_Demos.Age = input$Age,
      PreInt_EduHx.computerinternet_hoursday = input$InternetHours,
      SDS.SDS_Total_T = input$SDS_Total_T,
      PCIAT.PCIAT_01 = input$PCIAT_01,
      PCIAT.PCIAT_02 = input$PCIAT_02,
      PCIAT.PCIAT_03 = input$PCIAT_03,
      PCIAT.PCIAT_04 = input$PCIAT_04,
      PCIAT.PCIAT_05 = input$PCIAT_05,
      PCIAT.PCIAT_06 = input$PCIAT_06,
      PCIAT.PCIAT_07 = input$PCIAT_07,
      PCIAT.PCIAT_08 = input$PCIAT_08,
      PCIAT.PCIAT_09 = input$PCIAT_09,
      PCIAT.PCIAT_10 = input$PCIAT_10,
      PCIAT.PCIAT_11 = input$PCIAT_11,
      PCIAT.PCIAT_12 = input$PCIAT_12,
      PCIAT.PCIAT_13 = input$PCIAT_13,
      PCIAT.PCIAT_14 = input$PCIAT_14,
      PCIAT.PCIAT_15 = input$PCIAT_15,
      PCIAT.PCIAT_16 = input$PCIAT_16,
      PCIAT.PCIAT_17 = input$PCIAT_17,
      PCIAT.PCIAT_18 = input$PCIAT_18,
      PCIAT.PCIAT_19 = input$PCIAT_19,
      PCIAT.PCIAT_20 = input$PCIAT_20
    )
  })
  
  output$prediction_output <- renderText({
    data_input <- user_data()
    prediction <- predict(rf_model, newdata = data_input)
    paste0("Predicted PCIAT Total Score is: ", round(prediction, 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
