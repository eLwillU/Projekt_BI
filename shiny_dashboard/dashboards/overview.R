source("scripts/clustering.R")
source("scripts/plots.R")
source("scripts/preprocessing.R")


get_overview_ui <- function() {
  return(tabsetPanel(type = "tabs",
                     tabPanel(
                       "Heatmaps",
                       fluidPage(
                         h1("Heatmaps"),
                         selectInput("showDendros", "Show Dendrograms",
                                     c(yes = "yes", no = "no")
                         ),
                         conditionalPanel(
                           condition = "input.showDendros == 'yes'",
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
                       "General",
                       fluidPage(
                         h1("General"),
                         box(plotlyOutput("plot3")),
                         box(plotlyOutput("plot4")),
                         box(plotlyOutput("plot5")),
                         box(plotlyOutput("plot6")),
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
                     
                     ))
}
## TODO: add plot 7
get_overview_Server <- function(input, output){
  
  # prep data
  clinical_data <- loadData("raw_clinical_data_unbalanced.RDS")
  all_data <- loadData("raw_data_unbalanced.RDS")
  gene_data <- loadData("raw_gene_data_unbalanced.RDS")
  
  gene_df_rownames <- get_gene_df_rownames()
  gene_matrix <- get_gene_matrix(all_data, gene_df_rownames)
  
  # Heatmaps
  output$heatmapDFC <- renderPlot({get_static_heatmap(gene_matrix, death_from_cancer = TRUE)})
  output$plotlyHeatmapDFC <- renderPlotly({ get_plotly_heatmap(gene_matrix, death_from_cancer = TRUE)})
  output$heatmapNoDFC <- renderPlot({get_static_heatmap(gene_matrix, death_from_cancer = FALSE)})
  output$plotlyHeatmapNoDFC <- renderPlotly({ get_plotly_heatmap(gene_matrix, death_from_cancer = FALSE)})
  
  # General plots
  output$plot3 <- renderPlotly(get_survival_by_cancertype_plot(clinical_data))
  output$plot4 <- renderPlotly(get_survival_by_cancer_or_disease(clinical_data))
  output$plot5 <- renderPlotly(get_death_from_cancer_with_avg_age(clinical_data))
  output$plot6 <- renderPlotly(get_cohort_pie_chart(clinical_data))
  output$plot7 <- renderPlotly(get_cohort_pie_chart(clinical_data))
  
  # PCA plots
  output$pca1 <- renderPlot(get_pca_scree_all_numeric(all_data))
  output$pca2 <- renderPlot(get_pca_scree_clinical_numeric(clinical_data))
  output$pca3 <- renderPlot(get_pca_scree_all_gene(gene_data))
  output$pca4 <- renderPlot(get_pca_scree_filtered_gene(gene_matrix))
  return(output)
}
