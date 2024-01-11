source("scripts/preprocessing.R")
library(caret)
library(e1071)
library(dplyr)
library(plotly)

### CLUSTERING
df <- get_raw_clinical_data(balance_data = FALSE)
#df <- get_raw_gene_data(balance_data = FALSE)
#df <- get_raw_data(balance_data = FALSE)

df <- subset(df, select = -cohort) # not useful
df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
numeric <- df %>% select(where(is.numeric))

cl <- kmeans(numeric, 2)
numeric <- numeric %>% mutate(cluster = cl$cluster, death_from_cancer = df$death_from_cancer)

numeric %>% group_by(cluster) %>% summarise(
  survival_months = mean(overall_survival_months),
  tumor_size = mean(tumor_size),
  lymph_nodes_examined_positive = mean(lymph_nodes_examined_positive)
  )

plot(numeric, col = cl$cluster)


