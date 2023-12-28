source("scripts/preprocessing.R")
source("scripts/models.R")

## get data
df <- get_raw_clinical_data(balance_data = FALSE)

train_logistic_clinical_model(df)

loaded_model <- readRDS(file = "models/clinical_logistic.rds")

summary(loaded_model$model)
