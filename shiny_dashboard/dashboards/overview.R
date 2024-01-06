source("scripts/clustering.R")
source("scripts/plots.R")
source("scripts/preprocessing.R")


get_overview_ui <- function() {
  return(tabsetPanel(type = "tabs",
                     tabPanel(
                       "Heatmaps",
                       fluidPage(
                         h1("Heatmaps"),
                         box(plotOutput("plot1")),
                         box(plotlyOutput("plot2")),
                       )
                     ),
                     tabPanel(
                       "General",
                       fluidPage(
                         h1("Overview"),
                         box(plotlyOutput("plot3")),
                         box(plotlyOutput("plot4")),
                         box(plotlyOutput("plot5")),
                         box(plotlyOutput("plot6")),
                         box(plotOutput("plot8")),
                         box(plotOutput("plot9")),
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
  clinical_data <- get_raw_clinical_data(balance_data = FALSE)
  all_data <- get_raw_data(balance_data = FALSE)
  gene_data <- get_raw_gene_data(balance_data = FALSE)
  
  gene_df_rownames <- get_gene_df_rownames()
  gene_matrix <- get_gene_matrix(all_data, gene_df_rownames)
  
  output$plot1 <- renderPlot({get_static_heatmap(gene_matrix)})
  output$plot2 <- renderPlotly({ get_plotly_heatmap(gene_matrix)})
  output$plot3 <- renderPlotly(get_survival_by_cancertype_plot(clinical_data))
  output$plot4 <- renderPlotly(get_survival_by_cancer_or_disease(clinical_data))
  output$plot5 <- renderPlotly(get_death_from_cancer_with_avg_age(clinical_data))
  output$plot6 <- renderPlotly(get_cohort_pie_chart(clinical_data))
  output$plot7 <- renderPlotly(get_cohort_pie_chart(clinical_data))
  output$plot8 <- renderPlot(get_dfc_dendrogram(all_data = all_data, gene_df_rownames = gene_df_rownames))
  output$plot9 <- renderPlot(get_not_dfc_dendrogram(all_data = all_data, gene_df_rownames = gene_df_rownames))
  
  # PCA plots
  output$pca1 <- renderPlot(get_pca_scree_all_numeric(all_data))
  output$pca2 <- renderPlot(get_pca_scree_clinical_numeric(clinical_data))
  output$pca3 <- renderPlot(get_pca_scree_all_gene(gene_data))
  output$pca4 <- renderPlot(get_pca_scree_filtered_gene(gene_matrix))
  return(output)
}
