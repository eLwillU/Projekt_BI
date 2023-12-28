source("scripts/preprocessing.R")
source("scripts/models.R")

## get data
df <- get_raw_clinical_data(balance_data = FALSE)

# logistic regression
loaded_model <- readRDS(file = "models/clinical_logistic.rds")

