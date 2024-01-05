source("scripts/preprocessing.R")
library(ggplot2)
library(ggformula)
library(ggeasy)
library(dplyr)
library(randomForest)
library(gplots)
library(tidyr)
library(plotly)

# Get genomic data
genomic_data <- get_raw_gene_data()

get_gene_rf_model <- function(){
  library(randomForest)
  loaded_model <- readRDS("models/gene_rf_death_from_cancer.rds")
  return(loaded_model)
}

# Random Forest for feature selection
randomForest_model <- get_gene_rf_model()
feature_importance <- importance(randomForest_model)

# Create Data Frame from mean decrease gini coefficents
gene_df <- data.frame(col2 = feature_importance)

get_static_heatmap <- function(minGini = 3){
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > minGini) %>%
    rownames()
  
  filtered_matrix <- data.matrix(genomic_data[,gene_df_rownames])
  fig <- heatmap(filtered_matrix)
  return(fig)
}

get_plotly_heatmap <- function(minGini = 3){
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > minGini) %>%
    rownames()
  
  filtered_data <- genomic_data %>%
    select(gene_df_rownames)
  matrix <- data.matrix(filtered_data)
  return(
    plot_ly(x= colnames(matrix), z = matrix, type = "heatmap", colors="Oranges") %>%
      layout(title="Gene heatmap",
             xaxis=list(
               title="Genes"
             ),
             yaxis=list(
               title="Samples"
             ))
      
    )
  
}

