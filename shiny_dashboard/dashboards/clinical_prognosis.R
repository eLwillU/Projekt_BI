# Create UI outside for better code readability
get_clinical_prognosis_ui <- function() {
  return(fluidPage(
    tags$head(tags$style(
      HTML(
        "
            .rounded-grey-box {
                background-color: #c6d0e5;
                border-radius: 15px;
                padding: 10px;
                margin: 10px;
            }
        "
      )
    )),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Prediction",
        ## Patient Data inputs
        h1("Patient information"),
        fluidRow(
          # Age at diagnosis
          column(4,
                 numericInput("ageInput",
                              h3("Patient Age"),
                              value = 65)),
          # Type of surgery
          column(
            4,
            selectInput(
              "surgerytypeInput",
              h3("Type of surgery"),
              choices = list("BREAST CONSERVING" = "BREAST CONSERVING",
                             "MASTECTOMY" = "MASTECTOMY"),
              selected = "MASTECTOMY"
            )
          ),
          # Cancer Type
          column(
            4,
            selectInput(
              "cancertypeInput",
              h3("Cancer Type"),
              choices = list(
                "Breast Invasive Ductal Carcinoma" = "Breast Invasive Ductal Carcinoma",
                "Breast Invasive Lobular Carcinoma" = "Breast Invasive Lobular Carcinoma",
                "Breast Invasive Mixed Mucinous Carcinoma" = "Breast Invasive Mixed Mucinous Carcinoma",
                "Breast Mixed Ductal and Lobular Carcinoma" = "Breast Mixed Ductal and Lobular Carcinoma"
              ),
              selected = "Breast Invasive Ductal Carcinoma"
            )
          ),
          # cellularity
          column(
            4,
            selectInput(
              "cellularityInput",
              h3("Cellularity"),
              choices = list(
                "Low" = "Low",
                "Moderate" = "Moderate",
                "High" = "High"
              ),
              selected = "Moderate"
            )
          ),
          # chemotherapy
          column(
            4,
            selectInput(
              "chemotherapyInput",
              h3("Chemotherapy"),
              choices = list("yes" = "yes",
                             "no" = "no"),
              selected = "yes"
            )
          ),
          # pam50
          column(
            4,
            selectInput(
              "pam50Input",
              h3("PAM50 Test"),
              choices = list(
                "Basal" = "Basal",
                "claudin-low" = "claudin-low",
                "Her2" = "Her2",
                "LumA" = "LumA",
                "LumB" = "LumB",
                "NC" = "NC",
                "Normal" = "Normal"
              ),
              selected = "Basal"
            )
          ),
          # ER Status
          column(
            4,
            selectInput(
              "erInput",
              h3("ER Status"),
              choices = list("Negative" = "Negative",
                             "Positive" = "Positive"),
              selected = "Negative"
            )
          ),
          # Neoplasm Histologic Grade
          column(
            4,
            selectInput(
              "neoplasmInput",
              h3("Neoplasm Histologic Grade"),
              choices = list("1" = 1,
                             "2" = 2,
                             "3" = 3),
              selected = 2
            )
          ),
          # HER2 Status
          column(
            4,
            selectInput(
              "her2Input",
              h3("HER2"),
              choices = list("Negative" = "Negative",
                             "Positive" = "Positive"),
              selected = "Negative"
            )
          ),
          # Histologic Subtype
          column(
            4,
            selectInput(
              "histologicsubtypeInput",
              h3("Histologic Subtype"),
              choices = list(
                "Ductal/NST" = "Ductal/NST",
                "Lobular" = "Lobular",
                "Medullary" = "Medullary",
                "Mixed" = "Mixed",
                "Mucinous" = "Mucinous",
                "Tubular/ cribriform" = "Tubular/ cribriform"
              ),
              selected = "Mixed"
            )
          ),
          # Hormone Therapy
          column(
            4,
            selectInput(
              "hormoneInput",
              h3("Hormone Therapy"),
              choices = list("no" = "no",
                             "yes" = "yes"),
              selected = "yes"
            )
          ),
          # Inferred Menopausal State
          column(
            4,
            selectInput(
              "menopausalstateInput",
              h3("Inferred Menopausal State"),
              choices = list("Pre" = "Pre",
                             "Post" = "Post"),
              selected = "Post"
            )
          ),
          # Primary Tumor Laterality
          column(
            4,
            selectInput(
              "lateralityInput",
              h3("Primary Tumor Laterality"),
              choices = list("Left" = "Left",
                             "Right" = "Right"),
              selected = "Left"
            )
          ),
          # Lymphnodes
          column(4,
                 numericInput(
                   "lymphnodesInput",
                   h3("Lymphnodes"),
                   value = 0
                 )),
          # Mutation Count
          column(4,
                 numericInput(
                   "mutationcountInput",
                   h3("Mutation Count"),
                   value = 0
                 )),
          # Nottingham Prognostic Index
          column(4,
                 numericInput(
                   "nottinghamInput",
                   h3("Nottingham Index"),
                   value = 0
                 )),
          # PR Status
          column(
            4,
            selectInput(
              "prstatusInput",
              h3("PR Status"),
              choices = list("Positive" = "Positive",
                             "Negative" = "Negative"),
              selected = "Positive"
            )
          ),
          # Radio Therapy
          column(
            4,
            selectInput(
              "radiotherapyInput",
              h3("Radio Therapy"),
              choices = list("no" = "no",
                             "yes" = "yes"),
              selected = "yes"
            )
          ),
          # Tumor Size
          column(
            4,
            sliderInput(
              "tumorsizeInput",
              h3("Tumor Size"),
              min = 0,
              max = 200,
              value = 50
            )
          ),
        ),
        
        
        ## Display model predictions
        h1("Model predictions"),
        fluidRow(
          # logistic regression
          column(6,
                 div(
                   class = "rounded-grey-box",
                   uiOutput("logisticModelHeader"),
                   uiOutput("logisticModelOutput")
                 ),),
          # Naive Bayes
          column(6,
                 div(
                   class = "rounded-grey-box",
                   uiOutput("nbModelHeader"),
                   uiOutput("nbModelOutput")
                 ),),
          # Decision Tree
          column(6,
                 div(
                   class = "rounded-grey-box",
                   uiOutput("decisionTreeModelHeader"),
                   uiOutput("decisionTreeModelOutput")
                 ),),
          # Random Forest
          column(6,
                 div(
                   class = "rounded-grey-box",
                   uiOutput("rfModelHeader"),
                   uiOutput("rfModelOutput")
                 ),)
        )
      ),
      tabPanel(
        "Model Plots",
        box(plotOutput("rfClinicalPlot")),
        box(plotOutput("rfAllPlot")),
        )
    ),
  ))
}

# Create Server outside for better code readability
get_clinical_prognosis_server <- function(input, output) {
  # clinical prognosis
  source("scripts/models.R")
  ## patient data
  new_patient <- reactive({
    data.frame(
      age_at_diagnosis = input$ageInput,
      type_of_breast_surgery = input$surgerytypeInput,
      cancer_type_detailed = input$cancertypeInput,
      cellularity = input$cellularityInput,
      chemotherapy = input$chemotherapyInput,
      pam50_._claudin.low_subtype = input$pam50Input,
      er_status = input$erInput,
      neoplasm_histologic_grade = input$neoplasmInput,
      her2_status = input$her2Input,
      tumor_other_histologic_subtype = input$histologicsubtypeInput,
      hormone_therapy = input$hormoneInput,
      inferred_menopausal_state = input$menopausalstateInput,
      primary_tumor_laterality = input$lateralityInput,
      lymph_nodes_examined_positive = input$lymphnodesInput,
      mutation_count = input$mutationcountInput,
      nottingham_prognostic_index = input$nottinghamInput,
      pr_status = input$prstatusInput,
      radio_therapy = input$radiotherapyInput,
      tumor_size = input$tumorsizeInput
    )
  })
  
  
  ## logistic model
  clinical_logistic_model <- get_logistic_clinical_model_survival()
  output$logisticModelHeader <- renderUI({
    h2(paste(
      "Logistic Model
               [Sensitivity = ",
      round(clinical_logistic_model$sensitivity, 2),
      "Specificity = ",
      round(clinical_logistic_model$specificity, 2),
      "]"
    ))
  })
  output$logisticModelOutput <- renderUI({
    predicted_probabilities <-
      predict(clinical_logistic_model$model, new_patient(), type = "response")
    predicted_class <-
      ifelse(predicted_probabilities > 0.5, "Dies", "Survives")
    h3(paste(
      predicted_class,
      "[",
      round(predicted_probabilities, 3) * 100,
      "%]"
    ))
  })
  
  
  ## Naive Bayes
  nb_model <- get_clinical_nb_model_survival()
  output$nbModelHeader <- renderUI({
    h2(paste(
      "Naive Bayes Model
               [Sensitivity = ",
      round(nb_model$sensitivity, 2),
      "Specificity = ",
      round(nb_model$specificity, 2),
      "]"
    ))
  })
  output$nbModelOutput <- renderUI({
    predicted_probabilities <-
      predict(nb_model$model, new_patient(), type = "raw")
    predicted_class <-
      predict(nb_model$model, new_patient(), type = "class")
    output <- ""
    if (predicted_class == "yes") {
      output <-
        paste("Dies",
              "[",
              round(predicted_probabilities[, "yes"], 3) * 100,
              "%]")
    }
    else {
      output <-
        paste("Survives",
              "[",
              round(predicted_probabilities[, "no"], 3) * 100,
              "%]")
    }
    h3(output)
  })
  
  
  ## Decision Tree
  tree_model <- get_clinical_dectree_model_survival()
  output$decisionTreeModelHeader <- renderUI({
    h2(paste(
      "Decision Tree Model
               [Sensitivity = ",
      round(tree_model$sensitivity, 2),
      "Specificity = ",
      round(tree_model$specificity, 2),
      "]"
    ))
  })
  output$decisionTreeModelOutput <- renderUI({
    predicted_probabilities <-
      predict(tree_model$model, new_patient(), type = "prob")
    predicted_class <-
      predict(tree_model$model, new_patient(), type = "class")
    output <- ""
    if (predicted_class == "yes") {
      output <-
        paste("Dies",
              "[",
              round(predicted_probabilities[, "yes"], 3) * 100,
              "%]")
    }
    else {
      output <-
        paste("Survives",
              "[",
              round(predicted_probabilities[, "no"], 3) * 100,
              "%]")
    }
    h3(output)
  })
  
  ## Random Forest
  rf_model <- get_clinical_rftree_model_survival()
  rf_all_model <- readRDS(file = "models/all_rftree_model.rds")
  
  # Plots
  output$rfClinicalPlot <-
    renderPlot({
      library(randomForest)
      ?varImpPlot
      varImpPlot(rf_model$model$finalModel, main = "Random Forest with clinical data")
    })
  output$rfAllPlot <-
    renderPlot({
      varImpPlot(rf_all_model$model$finalModel, main = "Random Forest with all data")
    })
  
  output$rfModelHeader <- renderUI({
    h2(paste(
      "Random Forest Model
               [Sensitivity = ",
      round(rf_model$sensitivity, 2),
      "Specificity = ",
      round(rf_model$specificity, 2),
      "]"
    ))
  })
  output$rfModelOutput <- renderUI({
    predicted_probabilities <-
      predict(rf_model$model, new_patient(), type = "prob")
    predicted_class <-
      predict(rf_model$model, new_patient(), type = "raw")
    output <- ""
    if (predicted_class == "yes") {
      output <-
        paste("Dies",
              "[",
              round(predicted_probabilities[, "yes"], 3) * 100,
              "%]")
    }
    else {
      output <-
        paste("Survives",
              "[",
              round(predicted_probabilities[, "no"], 3) * 100,
              "%]")
    }
    h3(output)
  })
  
  return(output)
}