library(shiny)
library(randomForest)

# Load the trained Random Forest model
rf_model <- readRDS("random_forest_model.rds") 

# Define the UI
ui <- fluidPage(
  titlePanel("Problematic Internet Use Prediction (PCIAT Total Score)"),
  
  # Panel 1: Basic Info
  mainPanel(
    width = 8,  
    style = "margin-left: auto; margin-right: auto; margin-top: 20px;", 
    wellPanel(
      h3("Basic Information"),
      numericInput("Age", "Age:", value = 20, min = 5, max = 25),
      numericInput("InternetHours", "Computer/Internet Hours Per Day:", value = 4, min = 0, max = 24),
      numericInput("SDS_Total_T", "SDS Total T Score:", value = 50, min = 0, max = 100)
    )
  ),
  
  # Panel 2: PCIAT Questions
  mainPanel(
    width = 8, 
    style = "margin-left: auto; margin-right: auto; margin-top: 20px;", 
    wellPanel(
      h3("PCIAT Questionnaire"),
      numericInput("PCIAT_01", "How often does your child disobey time limits you set for online use? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_02", "How often does your child neglect household chores to spend more time online? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_03", "How often does your child prefer to spend time online rather than with the rest of your family? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_04", "How often does your child form new relationships with fellow online users? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_05", "How often do you complain about the amount of time your child spends online? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_06", "How often do your child's grades suffer because of the amount of time he or she spends online? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_07", "How often does your child check his or her email before doing something else? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_08", "How often does your child seem withdrawn from others since discovering the Internet? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_09", "How often does your child become defensive or secretive when asked what they do online? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_10", "How often have you caught your child sneaking online against your wishes? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_11", "How often does your child spend time alone in their room playing on the computer? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_12", "How often does your child receive strange phone calls from new 'online' friends? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_13", "How often does your child snap, yell, or act annoyed if bothered while online? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_14", "How often does your child seem more tired and fatigued than before discovering the Internet? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_15", "How often does your child seem preoccupied with being back online when offline? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_16", "How often does your child throw tantrums when you interfere with their online time? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_17", "How often does your child choose to spend time online instead of hobbies/activities they once enjoyed? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_18", "How often does your child become angry when you place time limits on online use? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_19", "How often does your child prefer spending time online over going out with friends? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5),
      numericInput("PCIAT_20", "How often does your child feel depressed, moody, or nervous offline but better once back online? (0=Does Not Apply to 5=Always)", value = 3, min = 0, max = 5)
    )
  ),
  
  # Panel 3: Prediction Section
  mainPanel(
    width = 8, 
    style = "margin-left: auto; margin-right: auto; margin-top: 20px;", 
    wellPanel(
      h3("Prediction"),
      actionButton("predict_btn", "Predict PCIAT Total", class = "btn-primary"),
      br(), br(),
      h3(textOutput("prediction_output"))
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
    paste0(round(prediction, 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
