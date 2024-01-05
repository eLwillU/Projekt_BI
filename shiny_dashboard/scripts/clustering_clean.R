source("scripts/preprocessing.R")
library(ggplot2)
library(ggformula)
library(ggeasy)
library(dplyr)
library(randomForest)
library(gplots)
library(tidyr)
library(plotly)
library(ggdendro)

# Load all data
gene_data <- load_all_data()


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

get_static_heatmap <- function(minGini = 4){
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > minGini) %>%
    rownames()
  
  filtered_matrix <- data.matrix(gene_data[,gene_df_rownames])
  fig <- heatmap(filtered_matrix)
  return(fig)
}

get_plotly_heatmap <- function(minGini = 4){
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > minGini) %>%
    rownames()
  
  filtered_data <- gene_data %>%
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

get_dfc_dendrogram <- function(minGini = 4){
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > minGini) %>%
    rownames()

    title <- "Death from cancer"
  cluster_data_death_from_cancer <- gene_data %>%
    select(death_from_cancer, gene_df_rownames) %>%
    filter(death_from_cancer == "yes") %>%
    select(gene_df_rownames)
 
  distances <- dist(cluster_data_death_from_cancer, method = "euclidean")
  clusterGenes <- hclust(distances, method="ward.D2")
  plot(clusterGenes)
  p <- ggdendrogram(clusterGenes, rotate = FALSE, size = 20) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  return (p)
}


get_not_dfc_dendrogram <- function(minGini = 4){
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > minGini) %>%
    rownames()
  
  
    title <- "Not death from cancer"
    cluster_data_death_from_cancer <- gene_data %>%
      select(death_from_cancer, gene_df_rownames) %>%
      filter(death_from_cancer == "no") %>%
      select(gene_df_rownames) 

  
  distances <- dist(cluster_data_death_from_cancer, method = "euclidean")
  clusterGenes <- hclust(distances, method="ward.D2")
  plot(clusterGenes)
  p <- ggdendrogram(clusterGenes, rotate = FALSE, size = 20) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  return (p)

  
}

# TODO: Cluster groups
# clustergroups <- cutree(clusterGenes, k=5)
# tapply(cluster_data$aurka, clustergroups, mean)


