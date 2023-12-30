source("scripts/preprocessing.R")
source("scripts/models.R")

df <- get_raw_clinical_data()
model <- get_logistic_clinical_model_survival()
colnames(df)
?predict
summary(df$cellularity)
