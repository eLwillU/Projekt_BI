source("scripts/preprocessing.R")
source("scripts/models.R")
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

get_gene_df_rownames <- function() {
  # Random Forest for feature selection
  gene_randomForest_model <- get_gene_rf_model()
  gene_feature_importance <- importance(gene_randomForest_model)
  
  # Create Data Frame from mean decrease gini coefficents
  gene_df <- data.frame(col2 = gene_feature_importance)
  
  # Get rownames of genes
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > 4) %>%
    rownames()

  return(gene_df_rownames)
}

get_gene_df <- function(all_data, gene_df_rownames) {
  # Filter the data by rownames
  filtered_gene_data <- all_data %>%
    dplyr::select(all_of(gene_df_rownames), death_from_cancer)
  return(filtered_gene_data)
}


get_static_heatmap <- function(gene_df, death_from_cancer = TRUE){
  if(death_from_cancer) {
    gene_df <- gene_df %>% 
      dplyr::filter(death_from_cancer == "yes") %>%
      select(-death_from_cancer)
    gene_matrix <- as.matrix(gene_df)
    fig <- heatmap(gene_matrix, xlab = "Genes", ylab= "Samples", main="Gene Heatmap (Death from Cancer)")
    return(fig)
  } 
  else {
    gene_df <- gene_df %>% 
      dplyr::filter(death_from_cancer == "no") %>%
      select(-death_from_cancer)
    gene_matrix <- as.matrix(gene_df)
    fig <- heatmap(gene_matrix, xlab = "Genes", ylab= "Samples", main="Gene Heatmap (Survived Cancer)")
    return(fig)
  }
}

get_plotly_heatmap <- function(gene_df, death_from_cancer = TRUE){
  
  if(death_from_cancer) {
    gene_df <- gene_df %>% 
      dplyr::filter(death_from_cancer == "yes") %>%
      select(-death_from_cancer)
    gene_matrix <- as.matrix(gene_df)
    
    return(
      plot_ly(x= colnames(gene_matrix), z = gene_matrix, type = "heatmap", colors="Oranges") %>%
        layout(title="Gene Heatmap (Death from Cancer)",
               xaxis=list(
                 title="Genes"
               ),
               yaxis=list(
                 title="Samples"
               )
        )
    )
  } 
  
  else {
    gene_df <- gene_df %>% 
      dplyr::filter(death_from_cancer == "no") %>%
      select(-death_from_cancer)
    gene_matrix <- as.matrix(gene_df)
    
    return(
      plot_ly(x= colnames(gene_matrix), z = gene_matrix, type = "heatmap", colors="Oranges") %>%
        layout(title="Gene Heatmap (Survived Cancer)",
               xaxis=list(
                 title="Genes"
               ),
               yaxis=list(
                 title="Samples"
               )
        )
    )
  }
}

# UNUSED
get_dfc_dendrogram <- function(all_data, gene_df_rownames, minGini = 4){
  title <- "Death from cancer"
  cluster_data_death_from_cancer <- all_data %>%
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

# UNUSED
get_not_dfc_dendrogram <- function(all_data, gene_df_rownames, minGini = 4){
    title <- "Not death from cancer"
    cluster_data_death_from_cancer <- all_data %>%
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

# pca scree all numeric data
get_pca_scree_all_numeric <- function(all_data){
  numeric_df <- all_data %>% select_if(is.numeric)
  pca <- prcomp(numeric_df, scale=F)
  pca.var <- pca$sdev^2
  pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
  p<- fviz_eig(pca, addlabels=T, main = "PCA for all the data")
  return(p)
}

# pca scree all numeric clinical data
get_pca_scree_clinical_numeric <- function(clinical_data){
  numeric_clinical_df <- clinical_data %>% select_if(is.numeric)
  pca <- prcomp(numeric_clinical_df, scale=F)
  pca.var <- pca$sdev^2
  pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
  p<- fviz_eig(pca, addlabels=T, main = "PCA for clinical data")
  return(p)
}

# pca scree all gene data
get_pca_scree_all_gene <- function(gene_data){
  numeric_gene_df <- gene_data %>% select_if(is.numeric)
  pca <- prcomp(numeric_gene_df, scale=F)
  pca.var <- pca$sdev^2
  pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
  p<- fviz_eig(pca, addlabels=T, main = "PCA for all gene data")
  return(p)
}

# pca scree filtered gene data
get_pca_scree_filtered_gene <- function(gene_matrix){
  pca <- prcomp(gene_matrix, scale=F)
  pca.var <- pca$sdev^2
  pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
  p<- fviz_eig(pca, addlabels=T, main = "PCA for filtered gene data")
  return(p)
}
