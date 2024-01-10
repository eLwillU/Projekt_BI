source("scripts/clustering.R")
source("scripts/plots.R")
source("scripts/preprocessing.R")


get_overview_ui <- function() {
  return(fluidPage(
    ### General
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "General",
        fluidPage(
          # input elements
          fluidRow(
            column(
              4,
              checkboxGroupInput(
                "selectedPlots",
                "Select Plots to Show",
                inline = TRUE,
                choices = list(
                  "Boxplots" = "showBoxplots",
                  "Piecharts" = "showPiecharts",
                  "Linecharts" = "showLinecharts"
                ),
                selected = list("showBoxplots", "showPiecharts")
              ),
            ),
            column(
              4,
              sliderInput(
                "ageRangeSlider",
                "Age:",
                min = 20,
                max = 100,
                value = c(20, 100)
              ), 
            ),
            column(
              4,
              sliderInput(
                "nottinghamIndexSlider",
                "Nottingham Prognostic Index:",
                min = 2,
                max = 7,
                value = c(2, 7)
              ), 
            )
          ),
          
          # boxplots
          conditionalPanel(
            condition = "input.selectedPlots.includes('showBoxplots')",
            h1("Boxplots"),
            box(plotlyOutput("cancerTypeBoxplot")),
            box(plotlyOutput("survivalBoxplot")),
            box(plotlyOutput("ageBoxplot")),
            box(plotlyOutput("lymphnodesBoxplot")),
            box(plotlyOutput("mutationcountBoxplot")),
            box(plotlyOutput("tumorSizeBoxplot")),
          ),
          
          # piecharts
          conditionalPanel(
            condition = "input.selectedPlots.includes('showPiecharts')",
            h1("Piecharts"),
            box(plotlyOutput("cancerTypePiechart")),
            box(plotlyOutput("deathPiechart")),
            box(plotlyOutput("surgeryTypePiechart")),
            box(plotlyOutput("chemoPiechart")),
            box(plotlyOutput("hormonePiechart")),
            box(plotlyOutput("radioPiechart")),
            box(plotlyOutput("menopausePiechart")),
            box(plotlyOutput("cohortPiechart")),
          ),
          
          # linecharts
          conditionalPanel(
            condition = "input.selectedPlots.includes('showLinecharts')",
            h1("Linecharts"),
            box(plotlyOutput("lymphnodesLinechart")),
            box(plotlyOutput("mutationsLinechart")),
            box(plotlyOutput("ageLinechart")),
          )
        )
      ),
      
      
      ### Heatmaps
      tabPanel(
        "Heatmaps",
        fluidPage(
          h1("Heatmaps"),
          selectInput("showDendros", "Show Dendrograms",
                      c("Yes" = "yes", "No" = "no")),
          conditionalPanel(condition = "input.showDendros == 'yes'",
                           box(plotOutput("heatmapDFC")),
                           box(plotOutput("heatmapNoDFC")),),
          conditionalPanel(
            # TODO: Plots without dendrogram
            condition = "input.showDendros == 'no'",
            box(plotlyOutput("plotlyHeatmapDFC")),
            box(plotlyOutput("plotlyHeatmapNoDFC")),
            box(plotOutput("plotHeatmapNoDendroDFC")),
            box(plotOutput("plotHeatmapNoDendro")),
          ),
          
        )
      ),
      
      
      ### PCA plots
      tabPanel("PCA",
               fluidPage(
                 h1("PCA Analysis"),
                 checkboxInput("scaleData", "Scale and Center data", value = FALSE),
                 box(plotOutput("pca1")),
                 box(plotOutput("pca2")),
                 box(plotOutput("pca3")),
                 box(plotOutput("pca4")),
               )),
      
      
      ### QQ-Plots
      tabPanel(
        "QQ-Plots",
        fluidPage(
          h1("QQ-Plots (test for normal distribution)"),
          checkboxInput("showHistograms", "Show Histograms", value = FALSE),
          # clinical data
          box(plotlyOutput("age_at_diagnosisQQ")),
          box(plotlyOutput("lymph_nodes_examined_positiveQQ")),
          box(plotlyOutput("overall_survival_monthsQQ")),
          box(plotlyOutput("tumor_sizeQQ")),
          # genes
          box(plotlyOutput("brca1QQ")),
          box(plotlyOutput("hes6QQ")),
        )
      ),
    )
  ))
}

get_overview_Server <- function(input, output) {
  library(dplyr)
  # prep data
  clinical_data <- loadData("raw_clinical_data_unbalanced.RDS")
  all_data <- loadData("raw_data_unbalanced.RDS")
  gene_data <- loadData("raw_gene_data_unbalanced.RDS")
  gene_df_rownames <- get_gene_df_rownames()
  gene_df <- get_gene_df(all_data, gene_df_rownames)
  # make reactive df
  reactive_clinical_data <- reactive({
    filtered_df <- clinical_data %>%
      filter(age_at_diagnosis > input$ageRangeSlider[1], age_at_diagnosis < input$ageRangeSlider[2]) %>%
      filter(nottingham_prognostic_index > input$nottinghamIndexSlider[1], nottingham_prognostic_index < input$nottinghamIndexSlider[2])
    return(filtered_df)
  })

  ## General plots
  # Boxplots
  output$cancerTypeBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        reactive_clinical_data(),
        reactive_clinical_data()$cancer_type_detailed,
        reactive_clinical_data()$overall_survival_months,
        title = "Months alive by cancer type",
        xaxis = "Cancer type",
        yaxis = "Survival in months"
      )
    )
  output$survivalBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        reactive_clinical_data(),
        reactive_clinical_data()$death_from_cancer,
        reactive_clinical_data()$overall_survival_months,
        title = "Months alive by death from cancer",
        xaxis = "Death from cancer",
        yaxis = "Survival in months"
      )
    )
  output$ageBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        reactive_clinical_data(),
        reactive_clinical_data()$death_from_cancer,
        reactive_clinical_data()$age_at_diagnosis,
        title = "Age at diagnosis by death from cancer",
        xaxis = "Death from cancer",
        yaxis = "Age at diagnosis"
      )
    )
  output$lymphnodesBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        reactive_clinical_data(),
        reactive_clinical_data()$death_from_cancer,
        reactive_clinical_data()$lymph_nodes_examined_positive,
        title = "Months alive by lymphnodes examined positive",
        xaxis = "Death from cancer",
        yaxis = "Lymphnodes tested positive"
      )
    )
  output$mutationcountBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        reactive_clinical_data(),
        reactive_clinical_data()$death_from_cancer,
        reactive_clinical_data()$mutation_count,
        title = "Months alive by mutation count",
        xaxis = "Death from cancer",
        yaxis = "Mutation count"
      )
    )
  output$tumorSizeBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        reactive_clinical_data(),
        reactive_clinical_data()$death_from_cancer,
        reactive_clinical_data()$tumor_size,
        title = "Tumor size by death from cancer",
        xaxis = "Death from cancer",
        yaxis = "Tumor size"
      )
    )
  
  
  # Piecharts
  output$cancerTypePiechart <-
    renderPlotly(
      get_generic_piechart(
        reactive_clinical_data(),
        labels = reactive_clinical_data()$cancer_type_detailed,
        "Cancer type Distribution",
        labelType = "percent"
      )
    )
  output$deathPiechart <-
    renderPlotly(
      get_generic_piechart(
        reactive_clinical_data(),
        labels = reactive_clinical_data()$death_from_cancer,
        "Death by cancer",
        labelType = "percent"
      )
    )
  output$surgeryTypePiechart <-
    renderPlotly(
      get_generic_piechart(
        reactive_clinical_data(),
        labels = reactive_clinical_data()$type_of_breast_surgery,
        "Surgery type Distribution"
      )
    )
  output$chemoPiechart <-
    renderPlotly(
      get_generic_piechart(
        reactive_clinical_data(),
        labels = reactive_clinical_data()$chemotherapy,
        "Chemotherapy distribution"
      )
    )
  output$hormonePiechart <-
    renderPlotly(
      get_generic_piechart(
        reactive_clinical_data(),
        labels = reactive_clinical_data()$hormone_therapy,
        "Hormone-Therapy distribution"
      )
    )
  output$radioPiechart <-
    renderPlotly(
      get_generic_piechart(
        reactive_clinical_data(),
        labels = reactive_clinical_data()$radio_therapy,
        "Radio Therapy Distribution"
      )
    )
  output$menopausePiechart <-
    renderPlotly(
      get_generic_piechart(
        reactive_clinical_data(),
        labels = reactive_clinical_data()$inferred_menopausal_state,
        "Pre or Post Menopausal state"
      )
    )
  output$cohortPiechart <-
    renderPlotly(
      get_generic_piechart(reactive_clinical_data(), labels = reactive_clinical_data()$cohort, "Cohort Distribution")
    )
  
  # Linecharts
  output$lymphnodesLinechart <- renderPlotly(
    get_generic_linechart(
      reactive_clinical_data(),
      reactive_clinical_data()$overall_survival_months,
      reactive_clinical_data()$lymph_nodes_examined_positive,
      "Impact of positive examined lymphnodes on survival in months",
      "Survival in months",
      "Lymphnodes examined positive"
    )
  )
  output$mutationsLinechart <- renderPlotly(
    get_generic_linechart(
      reactive_clinical_data(),
      reactive_clinical_data()$overall_survival_months,
      reactive_clinical_data()$mutation_count,
      "Impact of mutations on survival in months",
      "Survival in months",
      "Amount of mutations"
    )
  )
  output$ageLinechart <- renderPlotly(
    get_generic_linechart(
      reactive_clinical_data(),
      reactive_clinical_data()$overall_survival_months,
      reactive_clinical_data()$age_at_diagnosis,
      "Impact of age on survival in months",
      "Survival in months",
      "Age at diagnosis"
    )
  )
  
  
  ## Heatmaps
  output$heatmapDFC <-
    renderPlot({
      get_static_heatmap(gene_df, death_from_cancer = TRUE)
    })
  output$plotlyHeatmapDFC <-
    renderPlotly({
      get_plotly_heatmap(gene_df, death_from_cancer = TRUE)
    })
  output$heatmapNoDFC <-
    renderPlot({
      get_static_heatmap(gene_df, death_from_cancer = FALSE)
    })
  output$plotlyHeatmapNoDFC <-
    renderPlotly({
      get_plotly_heatmap(gene_df, death_from_cancer = FALSE)
    })
  
  output$plotHeatmapNoDendroDFC <-
    renderPlot({
      get_static_heatmap_without_dendro(gene_df, death_from_cancer = TRUE)
    })
  
  output$plotHeatmapNoDendro <-
    renderPlot({
      get_static_heatmap_without_dendro(gene_df, death_from_cancer = FALSE)
    })
  
  
  ## PCA plots
  output$pca1 <-
    renderPlot(get_pca_scree_all_numeric(all_data, input$scaleData))
  output$pca2 <-
    renderPlot(get_pca_scree_clinical_numeric(clinical_data, input$scaleData))
  output$pca3 <-
    renderPlot(get_pca_scree_all_gene(gene_data, input$scaleData))
  output$pca4 <-
    renderPlot(get_pca_scree_filtered_gene(gene_df, input$scaleData))
  
  # QQPLots
  # clinical
  output$age_at_diagnosisQQ <- renderPlotly(
    get_generic_qqplot(
      clinical_data,
      clinical_data$age_at_diagnosis,
      "Age at diagnosis",
      histogram = input$showHistograms
    )
  )
  output$lymph_nodes_examined_positiveQQ <-
    renderPlotly(
      get_generic_qqplot(
        clinical_data,
        clinical_data$lymph_nodes_examined_positive,
        "Lymphnodes examined positive",
        histogram = input$showHistograms
      )
    )
  output$overall_survival_monthsQQ <-
    renderPlotly(
      get_generic_qqplot(
        clinical_data,
        clinical_data$overall_survival_months,
        "Survival in months",
        histogram = input$showHistograms
      )
    )
  output$tumor_sizeQQ <- renderPlotly(
    get_generic_qqplot(
      clinical_data,
      clinical_data$tumor_size,
      "Tumor size",
      histogram = input$showHistograms
    )
  )
  # genes
  output$brca1QQ <- renderPlotly(
    get_generic_qqplot(
      gene_data,
      gene_data$brca1,
      "BRCA1 Gene",
      histogram = input$showHistograms
    )
  )
  output$hes6QQ <- renderPlotly(
    get_generic_qqplot(
      gene_data,
      gene_data$hes6,
      "HES6 Gene",
      histogram = input$showHistograms
    )
  )
  
  return(output)
}
