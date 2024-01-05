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

# Random Forest for feature selection
randomForest_model <- randomForest(death_from_cancer ~ ., data = genomic_data)
feature_importance <- importance(randomForest_model)

# Create Data Frame from mean decrease gini coefficents
gene_df <- data.frame(col2 = feature_importance)


plot_gene_heatmap = function(minGini = 3){
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > minGini) %>%
    rownames()
  
  filtered_matrix <- data.matrix(genomic_data[,gene_df_rownames])
  fig <- heatmap(filtered_matrix)
  return(fig)
}



get_clustering_ui <- function(){
  return(
    fluidPage(
      h1("Overview")
    )
  )
}

get_clustering_server <- function(input, output){
  output$plot1 <- renderPlot(
    {
      plot_gene_heatmap(4)
    }
  )
  return(output)
  
}

getwd()

randomForest_model
# TODO: Fix error
# RF-Model Death from cancer

