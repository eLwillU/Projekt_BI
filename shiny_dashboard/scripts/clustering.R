source("scripts/preprocessing.R")
library(ggplot2)
library(ggformula)
library(ggeasy)
library(dplyr)
library(randomForest)
library(gplots)
library(tidyr)
library(plotly)


data <- get_raw_clinical_data()


your_data_numeric <- model.matrix(death_from_cancer ~ ., data)[,-1]
rf_model <- randomForest(death_from_cancer ~ ., data = data)
feature_importance <- importance(rf_model)
significant_columns <- rownames(feature_importance)

df <- data.frame(col2 = feature_importance)

df <- df %>%
  arrange(desc(MeanDecreaseGini))

df


data2 <- get_raw_data()

sum(data2$mutation_count)

# Get genomic data
genomic_data <- get_raw_gene_data()
 
# Random Forest for feature selection
randomForest_model <- randomForest(death_from_cancer ~ ., data = genomic_data)
feature_importance <- importance(randomForest_model)

# Create Data Frame from mean decrease gini coefficents
gene_df <- data.frame(col2 = feature_importance)

# Select and filter most important rows and plot a heatmap
plot_gene_heatmap = function(minGini = 3){
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > minGini) %>%
    rownames()
  
  filtered_matrix <- data.matrix(genomic_data[,gene_df_rownames])
  heatmap(filtered_matrix)
}

plot_gene_heatmap(3.5)

ggplot(filtered_matrix, aes(x = ) ) +
  geom_tile()


distances <- dist(t(filtered_matrix))
hc <- hclust(distances)
plot <- as.dendrogram(hc)
plot(plot)


matrix <- data.matrix(genomic_data)
heatmap(matrix)




distc <- dist(matrix, method = "euclidean")
distr <- dist(t(matrix), method = "euclidean")
hc <- hclust(distc)
hr <- hclust(distr)

?heatmap.2


means <- genomic_data %>%
  colMeans()
  
summary(genomic_data)

data.frame(V1=sort(means, decreasing=TRUE))
  
colMeans(genomic_data)


heatmap.2(matrix)




plot_gene_heatmap <- function(minGini = 3) {
  gene_df_rownames <- gene_df %>%
    arrange(desc(MeanDecreaseGini)) %>%
    filter(MeanDecreaseGini > minGini)
  
  
  filtered_data <- genomic_data %>%
    select(rownames = 1, gene_df_rownames) %>%
    gather(key = "gene", value = "value", -rownames) %>%
    plot()
  
  ggplot(filtered_data, aes(x = gene, y = rownames, fill = value)) +
    geom_tile() 
}

# Call the function with your desired minGini value
plot_gene_heatmap(minGini = 3)

gene_df_rownames <- gene_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  filter(MeanDecreaseGini > 4) %>%
  rownames()


filtered_data <- genomic_data %>%
  select(gene_df_rownames)

matrix <- data.matrix(filtered_data)



fig <- plot_ly(z = matrix,colors = "Greens", type = "heatmap")
heatmap(matrix)
heatmap.2(matrix)

fig


