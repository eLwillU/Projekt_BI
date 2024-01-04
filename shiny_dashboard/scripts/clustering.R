source("scripts/preprocessing.R")
library(ggplot2)
library(ggformula)
library(ggeasy)
library(dplyr)
library(randomForest)


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


install.packages("gplots")

library("gplots")


genomic_data <- get_raw_gene_data() %>% 
  select(-death_from_cancer)
 

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

