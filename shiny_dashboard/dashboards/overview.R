source("scripts/clustering.R")
source("scripts/plots.R")
source("scripts/preprocessing.R")


get_overview_ui <- function() {
  return(fluidPage(
    checkboxGroupInput(
      "selectedPlots",
      "Select Plots to Show",
      inline = TRUE,
      choices = list(
        "Boxplots" = "showBoxplots",
        "Piecharts" = "showPiecharts"
      ),
      selected = list("showBoxplots", "showPiecharts")
    ),
    
    tabsetPanel(type = "tabs",
                tabPanel(
                  "General",
                  fluidPage(
                    h1("General"),
                    conditionalPanel(
                      condition = "input.selectedPlots.includes('showBoxplots')",
                      box(plotlyOutput("cancerTypeBoxplot")),
                      box(plotlyOutput("survivalBoxplot")),
                      box(plotlyOutput("ageBoxplot")),
                      box(plotlyOutput("tumorSizeBoxplot")),
                    ),
                    conditionalPanel(
                      condition = "input.selectedPlots.includes('showPiecharts')",
                      box(plotlyOutput("cancerTypePiechart")),
                      box(plotlyOutput("surgeryTypePiechart")),
                      box(plotlyOutput("chemoPiechart")),
                      box(plotlyOutput("hormonePiechart")),
                      box(plotlyOutput("cohortPiechart")),
                    ),
                  )
                ),
                tabPanel(
                  "Heatmaps",
                  fluidPage(
                    h1("Heatmaps"),
                    selectInput("showDendros", "Show Dendrograms",
                                c(yes = "yes", no = "no")
                    ),
                    conditionalPanel(
                      condition = "input.showDendros == 'yes'",
                      checkboxInput("input.dendro", "Show dendrograms", F),
                      box(plotOutput("heatmapDFC")),
                      box(plotOutput("heatmapNoDFC")),
                    ),
                    conditionalPanel(
                      # TODO: Plots without dendrogram
                      condition = "input.showDendros == 'no'",
                      box(plotlyOutput("plotlyHeatmapDFC")),
                      box(plotlyOutput("plotlyHeatmapNoDFC")),
                    ),
                  )
                ),
                tabPanel(
                  "PCA",
                  fluidPage(
                    h1("PCA Analysis"),
                    # PCA plots
                    box(plotOutput("pca1")),
                    box(plotOutput("pca2")),
                    box(plotOutput("pca3")),
                    box(plotOutput("pca4")),
                  )
                ),
                
    )))
}

get_overview_Server <- function(input, output){
  
  # prep data
  clinical_data <- loadData("raw_clinical_data_unbalanced.RDS")
  all_data <- loadData("raw_data_unbalanced.RDS")
  gene_data <- loadData("raw_gene_data_unbalanced.RDS")
  gene_df_rownames <- get_gene_df_rownames()
  gene_df <- get_gene_df(all_data, gene_df_rownames)
  
  
  ## General plots
  # Boxplots
  output$cancerTypeBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        clinical_data,
        clinical_data$cancer_type_detailed,
        clinical_data$overall_survival_months,
        title = "Survival in months by cancer type",
        xaxis = "Cancer type",
        yaxis = "Survival in months"
      )
    )
  output$survivalBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        clinical_data,
        clinical_data$death_from_cancer,
        clinical_data$overall_survival_months,
        title = "Survival in months by survival from cancer",
        xaxis = "Death from cancer",
        yaxis = "Survival in months"
      )
    )
  output$ageBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        clinical_data,
        clinical_data$death_from_cancer,
        clinical_data$age_at_diagnosis,
        title = "Age at diagnosis",
        xaxis = "Death from cancer",
        yaxis = "Age at diagnosis"
      )
    )
  output$tumorSizeBoxplot <-
    renderPlotly(
      get_generic_boxplot(
        clinical_data,
        clinical_data$death_from_cancer,
        clinical_data$tumor_size,
        title = "Tumor Size by survival",
        xaxis = "Death from cancer",
        yaxis = "Tumor size"
      )
    )
  # Piecharts
  output$cancerTypePiechart <- renderPlotly(get_generic_piechart(clinical_data, labels=clinical_data$cancer_type_detailed, "Cancer Distribution", labelType = "percent"))
  output$surgeryTypePiechart <- renderPlotly(get_generic_piechart(clinical_data, labels=clinical_data$type_of_breast_surgery, "Surgery Distribution"))
  output$chemoPiechart <- renderPlotly(get_generic_piechart(clinical_data, labels=clinical_data$chemotherapy, "Chemotherapy Distribution"))
  output$hormonePiechart <- renderPlotly(get_generic_piechart(clinical_data, labels=clinical_data$hormone_therapy, "Hormone-Therapy Distribution"))
  output$cohortPiechart <- renderPlotly(get_generic_piechart(clinical_data, labels=clinical_data$cohort, "Cohort Distribution"))
  
  
  # Heatmaps
  output$heatmapDFC <- renderPlot({get_static_heatmap(gene_df, death_from_cancer = TRUE)})
  output$plotlyHeatmapDFC <- renderPlotly({ get_plotly_heatmap(gene_df, death_from_cancer = TRUE)})
  output$heatmapNoDFC <- renderPlot({get_static_heatmap(gene_df, death_from_cancer = FALSE)})
  output$plotlyHeatmapNoDFC <- renderPlotly({ get_plotly_heatmap(gene_df, death_from_cancer = FALSE)})
  
  # PCA plots
  output$pca1 <- renderPlot(get_pca_scree_all_numeric(all_data))
  output$pca2 <- renderPlot(get_pca_scree_clinical_numeric(clinical_data))
  output$pca3 <- renderPlot(get_pca_scree_all_gene(gene_data))
  output$pca4 <- renderPlot(get_pca_scree_filtered_gene(gene_df))
  return(output)
}
