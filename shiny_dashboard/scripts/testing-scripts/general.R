source("scripts/models.R")

rfm <- get_clinical_rftree_model_survival()
lgm <- get_logistic_clinical_model_survival()
summary(lgm$model)


plot(rfm$model$finalModel)
