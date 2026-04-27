library(shiny)
library(randomForest)
library(shinycssloaders)
library(plotly)
library(shinyalert)

# Load the trained Random Forest model
rf_model <- readRDS("random_forest_model.rds") 

# Define the UI
ui <- fluidPage(
  
  useShinyalert(), # for popup
  titlePanel("Problematic Internet Use Predictor"),
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;700&display=swap');
      body {
        background-color: #f0f8f5;
        font-family: 'Poppins', sans-serif;
        color: #2e4d38;
      }
      .well {
        background-color: #ffffff;
        border-radius: 15px;
        padding: 25px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.05);
      }
      h3 {
        text-align: center;
        font-weight: 700;
        color: #1b4332;
        margin-bottom: 20px;
      }
      label {
        font-weight: 600;
      }
      .form-control {
        border-radius: 10px;
        border: 1px solid #ced4da;
        padding: 10px;
      }
      .form-control:focus {
        border-color: #2d6a4f;
        box-shadow: 0 0 8px rgba(46, 125, 50, 0.3);
      }
      .btn-primary {
        background-color: #2d6a4f;
        border-color: #2d6a4f;
        border-radius: 10px;
        padding: 10px 20px;
        font-size: 16px;
        font-weight: bold;
      }
      .btn-primary:hover {
        background-color: #40916c;
        transform: scale(1.05);
      }
      #prediction_output {
        font-size: 24px;
        font-weight: bold;
        color: #1b4332;
        text-align: center;
        margin-top: 20px;
      }
      #emoji_box {
        text-align: center;
        font-size: 30px;
        font-weight: bold;
        padding: 20px;
        border-radius: 10px;
        background-color: #ffffff;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        margin-top: 20px;
      }
      .fine { color: green; }
      .mild { color: yellow; }
      .high { color: red; }
    "))
  ),
  
  # Panel 1: Basic Info
  mainPanel(
    width = 12,  
    style = "margin-left: auto; margin-right: auto; margin-top: 20px;", 
    wellPanel(
      h3("Demo-Graphics"),
      numericInput("Age", "Age:", value = 20, min = 5, max = 25),
      numericInput("InternetHours", "Computer/Internet Hours Per Day:", value = 4, min = 0, max = 24),
      numericInput("SDS_Total_T", "SDS Total T Score:", value = 50, min = 0, max = 100),
      helpText("How would you rate your child's sleep on an average daily basis from 0 (poor) to 100 (excellent)?")
    )
  ),
  
  # Panel 2: PCIAT Questions
  mainPanel(
    width = 12, 
    style = "margin-left: auto; margin-right: auto; margin-top: 20px;", 
    wellPanel(
      h3("Parent Child Internet Addiction Test"),
      helpText("Answer based on a scale of 0–5: 0 = Does Not Apply, 1 = Rarely, 2 = Occasionally, 3 = Frequently, 4 = Often, 5 = Always."), 
      numericInput("PCIAT_01", "How often does your child disobey time limits you set for online use? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_02", "How often does your child neglect household chores to spend more time online? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_03", "How often does your child prefer to spend time online rather than with the rest of your family? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_04", "How often does your child form new relationships with fellow online users? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_05", "How often do you complain about the amount of time your child spends online? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_06", "How often do your child's grades suffer because of the amount of time he or she spends online? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_07", "How often does your child check his or her email before doing something else? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_08", "How often does your child seem withdrawn from others since discovering the Internet? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_09", "How often does your child become defensive or secretive when asked what they do online? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_10", "How often have you caught your child sneaking online against your wishes? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_11", "How often does your child spend time alone in their room playing on the computer? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_12", "How often does your child receive strange phone calls from new 'online' friends? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_13", "How often does your child snap, yell, or act annoyed if bothered while online? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_14", "How often does your child seem more tired and fatigued than before discovering the Internet? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_15", "How often does your child seem preoccupied with being back online when offline? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_16", "How often does your child throw tantrums when you interfere with their online time? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_17", "How often does your child choose to spend time online instead of hobbies/activities they once enjoyed? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_18", "How often does your child become angry when you place time limits on online use? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_19", "How often does your child prefer spending time online over going out with friends? ", value = 3, min = 0, max = 5),
      numericInput("PCIAT_20", "How often does your child feel depressed, moody, or nervous offline but better once back online? ", value = 3, min = 0, max = 5)
      
    )
  ),
  
  # Panel 3: Prediction Section
  mainPanel(
    width = 12, 
    style = "margin-left: auto; margin-right: auto; margin-top: 20px;", 
    wellPanel(
      h3("Prediction"),
      actionButton("predict_btn", "Predict PCIAT Total", class = "btn-primary"),
      br(), br(),
      withSpinner(textOutput("prediction_output"), type = 4, color = "#2d6a4f"),
      br(),
      plotlyOutput("gauge_plot"),  # gauge meter output
      div(id = "emoji_box", textOutput("emoji_text"))  # emoji and text box
    )
  )
)

# Define the Server
server <- function(input, output) {
  
  user_data <- reactive({
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
  
  prediction_result <- eventReactive(input$predict_btn, {
    Sys.sleep(1) # add 1 second delay
    data_input <- user_data()
    predict(rf_model, newdata = data_input)
  })
  
  output$prediction_output <- renderText({
    req(prediction_result())
    prediction_value <- round(prediction_result(), 2)
    
    # After prediction show popup
    shinyalert(
      title = "Prediction Successful!",
      text = "Your Child's Internet Usage Score has been predicted!",
      type = "success"
    )
    
    paste0("Predicted Internet Usage Score: ", prediction_value)
  })
  
  output$gauge_plot <- renderPlotly({
    req(prediction_result())
    prediction_value <- round(prediction_result(), 2)
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = prediction_value,
      gauge = list(
        axis = list(range = list(0, 100)), 
        steps = list(
          list(range = c(0, 40), color = "lightgreen"),   # Fine
          list(range = c(40, 70), color = "yellow"),      # Mild
          list(range = c(70, 100), color = "red")         # High
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 0.75,
          value = prediction_value
        )
      )
    )
  })
  
  output$emoji_text <- renderText({
  req(prediction_result())
  prediction_value <- round(prediction_result(), 2)
  
  # Emoji and text based on prediction zone
  if (prediction_value <= 40) {
    return("Low Risk  🧠")  # Plain text with Emoji for Fine
  } else if (prediction_value <= 70) {
    return("Moderate Risk ⚡")  # Plain text with Emoji for Mild
  } else {
    return("High Risk 🚨")  # Plain text with Emoji for High
  }
})
}

# Run the application 
shinyApp(ui = ui, server = server)
