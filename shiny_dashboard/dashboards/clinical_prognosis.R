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
        h1("Prognosis Dashboard"),
        p(
          "All clinical features from the dataset can be used to get a prediction if the patient survived. Four different models were trained. The percentage number does not have the same meaning for every model and is mainly visible to see the impact of changing the values. Genetic data was ignored because it introduced too many features"
        ),
        ## Patient Data inputs
        fluidRow(
          box(
            title = "Patient inputs",
            width = 12,
            collapsible = T,
            collapsed = F,
            # Age at diagnosis
            column(4,
                   numericInput("ageInput",
                                h3("Patient Age"),
                                value = 65)),
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
            )
          ),
          box(
            title = "Intervention inputs",
            width = 12,
            collapsible = T,
            collapsed = F,
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
            # chemotherapy
            column(
              4,
              selectInput(
                "chemotherapyInput",
                h3("Chemotherapy"),
                choices = list("yes" = "yes",
                               "no" = "no"),
                selected = "no"
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
                selected = "no"
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
                selected = "no"
              )
            )
          ),
          box(
            title = "Simple cancer inputs",
            width = 12,
            collapsible = T,
            collapsed = T,
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
                     value = 2
                   )),
            # Mutation Count
            column(4,
                   numericInput(
                     "mutationcountInput",
                     h3("Mutation Count"),
                     value = 5
                   )),
            # Nottingham Prognostic Index
            column(4,
                   numericInput(
                     "nottinghamInput",
                     h3("Nottingham Index"),
                     value = 4
                   )),
            # Tumor Size
            column(
              4,
              sliderInput(
                "tumorsizeInput",
                h3("Tumor Size"),
                min = 0,
                max = 200,
                value = 23
              )
            )
          ),
          box(
            title = "Detailed cancer inputs",
            width = 12,
            collapsible = T,
            collapsed = T,
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
                   uiOutput("logisticModelOutput"),
                   p(
                     "Logistic model trained on a subset of the data. Features were selected with forward / backward selection"
                   )
                 ), ),
          # Naive Bayes
          column(6,
                 div(
                   class = "rounded-grey-box",
                   uiOutput("nbModelHeader"),
                   uiOutput("nbModelOutput"),
                   p(
                     "Naive Bayes model trained on a subset of the data. Features were selected with the caret package and Recursive Feature Elimination (RFE)"
                   )
                 ), ),
          # Decision Tree
          column(6,
                 div(
                   class = "rounded-grey-box",
                   uiOutput("decisionTreeModelHeader"),
                   uiOutput("decisionTreeModelOutput"),
                   p(
                     "Decision Tree Model with all data included. Feature selection is done by the model itself"
                   )
                 ), ),
          # Random Forest
          column(6,
                 div(
                   class = "rounded-grey-box",
                   uiOutput("rfModelHeader"),
                   uiOutput("rfModelOutput"),
                   p(
                     "Random Forest Model with all data included. Feature selection is done by the model itself"
                   )
                 ), )
        )
      ),
      tabPanel(
        "Model Plots",
        h1("Plots to showcase the created models"),
        box(
          title = "Decision Tree plots",
          width = 12,
          collapsible = T,
          collapsed = F,
          fluidRow(
            column(
              12,
              p(
                "Showcase of feature importance and the decision tree itself. For visual reasons the decision tree plots features were reduced. Only features with a cp value of 0.007 or better were used"
              )
            ),
            column(12,
                   plotOutput("dectreePlot1"), ),
            column(12,
                   plotOutput("dectreePlot2"), )
          )
        ),
        box(
          title = "Random Forest plots",
          width = 12,
          collapsible = T,
          collapsed = F,
          fluidRow(column(
            12,
            p("Shows the feature importance of the random forest model.")
          ),),
          column(12,
                 plotOutput("rfClinicalPlot"))
        )
        
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
  
  # Plots
  
  library(rpart)
  library(rpart.plot)
  output$dectreePlot1 <- renderPlot({
    plotcp(tree_model$model)
  })
  
  output$dectreePlot2 <- renderPlot({
    pruned_tree <- prune(tree_model$model, cp = 0.0075935)
    plot2 <- prp(pruned_tree, roundint = FALSE)
  })
  
  output$rfClinicalPlot <-
    renderPlot({
      library(randomForest)
      ? varImpPlot
      varImpPlot(rf_model$model$finalModel, main = "Random Forest with clinical data")
    })
  
  return(output)
}