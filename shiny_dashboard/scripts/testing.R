source("scripts/preprocessing.R")
source("scripts/models.R")
library(caret)
library(dplyr)

df <- get_raw_data(balance_data = FALSE)

summary(df$death_from_cancer)

clinical <- get_raw_clinical_data()
gene <- get_raw_gene_data()

gene <- gene %>% select(-death_from_cancer)
