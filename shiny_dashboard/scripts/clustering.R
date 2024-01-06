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
library(factoextra)

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

# Get rownames of genes
gene_df_rownames <- gene_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  filter(MeanDecreaseGini > 4) %>%
  rownames()

# Filter the data by rownames

filtered_data <- gene_data %>%
  dplyr::select(gene_df_rownames)


# Create matrix
matrix <- data.matrix(filtered_data)

# Normalize matrix
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_matrix <- apply(matrix, 2, normalize)



get_static_heatmap <- function(){
  fig <- heatmap(matrix, xlab = "Genes", ylab= "Samples", main="Gene Heatmap")
  return(fig)
}

?plot_ly

get_plotly_heatmap <- function(){
   return(
    plot_ly(x= colnames(matrix), z = matrix, type = "heatmap", colors="Oranges") %>%
      layout(title="Gene heatmap",
             xaxis=list(
               title="Genes"
             ),
             yaxis=list(
               title="Samples"
             )
        )
    )
}

get_dfc_dendrogram <- function(minGini = 4){
  title <- "Death from cancer"
  cluster_data_death_from_cancer <- gene_data %>%
    dplyr::select(death_from_cancer, gene_df_rownames) %>%
    dplyr::filter(death_from_cancer == "yes") %>%
    dplyr::select(gene_df_rownames)
 
  distances <- dist(cluster_data_death_from_cancer, method = "euclidean")
  clusterGenes <- hclust(distances, method="ward.D2")
  plot(clusterGenes)
  p <- ggdendrogram(clusterGenes, rotate = FALSE, size = 20) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  return (p)
}


get_not_dfc_dendrogram <- function(minGini = 4){
    title <- "Not death from cancer"
    cluster_data_death_from_cancer <- gene_data %>%
      dplyr::select(death_from_cancer, gene_df_rownames) %>%
      dplyr::filter(death_from_cancer == "no") %>%
      dplyr::select(gene_df_rownames) 

  
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


get_pca_scree <- function(){
  pca <- prcomp(matrix, scale=F)
  pca.var <- pca$sdev^2
  pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
  
  p<- fviz_eig(pca,
               addlabels=T, main = "Scree Plot PCA")
  return(p)
}







