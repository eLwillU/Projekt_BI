library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Breast-Cancer Dashboard"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("house")),
    menuItem("Clinical Prognosis",tabName = "prognosis_clinical",icon = icon("person-dress"))
  )),
  
  dashboardBody(tabItems(
    ### clinical prognosis
    tabItem(tabName = "prognosis_clinical",
      fluidPage(
        ## Patient Data inputs
        h1("Patient information"),
        fluidRow(
          # cellularity
          column(4,
                 selectInput("selectCellularity", h3("Cellularity"), 
                             choices = list("Low" = "Low", 
                                            "Moderate" = "Moderate", 
                                            "High" = "High"), 
                             selected = "Moderate")),
          # pam50
          column(4,
                 selectInput("selectPAM50", h3("PAM50 Test"), 
                             choices = list("Basal" = "Basal", 
                                            "claudin-low" = "claudin-low", 
                                            "Her2" = "Her2", 
                                            "LumA" = "LumA", 
                                            "LumB" = "LumB", 
                                            "NC" = "NC", 
                                            "Normal" = "Normal"), 
                             selected = "Basal")),
          
          # HER Status
          column(4,
                 selectInput("selectHER2", h3("HER2"), 
                             choices = list("Negative" = "Negative", 
                                            "Positive" = "Positive"), 
                             selected = "Negative")),
          
          # Hormone Therapy
          column(4,
                 selectInput("selectHormone", h3("Hormone Therapy"), 
                             choices = list("no" = "no", 
                                            "yes" = "yes"), 
                             selected = "yes")),
          
          # Lymphnodes
          column(4, 
                 numericInput("lymphnodesNumber", 
                              h3("Lymphnodes"), 
                              value = 0)),
          
          # Nottingham Prognostic Index
          column(4, 
                 numericInput("nottinghamNumber", 
                              h3("Nottingham Index"), 
                              value = 0)),
          
          # Tumor Size
          column(4, 
                 sliderInput("tumorsizeSlider", h3("Tumor Size"),
                             min = 0, max = 200, value = 50)
          ),
          ),
        
        
        ## Display model predictions
        h1("Model predictions"),
        
        # logistic regression
        fluidRow(column(12,
                        uiOutput("logisticModelHeader")),
                 column(12,
                        uiOutput("logisticModelOutput"))
                 ), 
      )
    )
  )),
)


server <- function(input, output) {
  # clinical prognosis
  source("scripts/models.R")
  # logistic model
  clinical_logistic <- get_logistic_clinical_model_survival()
  output$logisticModelHeader <- renderUI({
    h2(paste("Logistic Model 
               [Sensitivity = ", round(clinical_logistic$sensitivity, 1),
               "Specificity = ", round(clinical_logistic$specificity, 1),
               "]"
               ))
  })
  output$logisticModelOutput <- renderUI({
    new_patient <- data.frame(
      cellularity = input$selectCellularity,  
      pam50_._claudin.low_subtype = input$selectPAM50,
      her2_status = input$selectHER2, 
      hormone_therapy = input$selectHormone,
      lymph_nodes_examined_positive = input$lymphnodesNumber,  
      nottingham_prognostic_index = input$nottinghamNumber,  
      tumor_size = input$tumorsizeSlider  
    )
    predicted_probabilities <- predict(clinical_logistic$model, new_patient, type = "response")
    predicted_class <- ifelse(predicted_probabilities > 0.5, "Dies", "Survives")
    h3(paste(predicted_class, "[", round(predicted_probabilities, 3)*100, "%]"))
  })
}


shinyApp(ui, server)
