save_model <- function(name, confMatrix, model) {
  model_list <- list(
    sensitivity = confMatrix$byClass["Sensitivity"],
    specificity = confMatrix$byClass["Specificity"],
    balanced_accuracy = confMatrix$byClass["Balanced Accuracy"],
    kappa = confMatrix$byClass["kappa"],
    model = model
  )
  
  saveRDS(model_list, file = paste0("models/", "", name))
}

## logistic models
train_logistic_clinical_model_survival <- function(df) {
  library(caret)
  df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
  df <- subset(df, select = -cohort) # not useful
  df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
  
  ## split data
  trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)
  trainData <- df[trainIndex, ]
  testData <- df[-trainIndex, ]
  # make model
  model <- glm(death_from_cancer ~ ., data = trainData, family = 'binomial')
  model_stepped <- step(model, trace = FALSE, direction= "both")
  print(summary(model_stepped))
  # test model
  probabilities <- predict(model_stepped, newdata = testData, type = 'response')
  predicted_classes <- ifelse(probabilities > 0.5, "yes", "no")
  predicted_classes <- factor(predicted_classes, levels = c("no", "yes"))
  confMatrix <- confusionMatrix(predicted_classes, testData$death_from_cancer)
  print(confMatrix)
  
  save_model("clinical_logistic_survival.rds", confMatrix = confMatrix, model=model_stepped)
}

get_logistic_clinical_model_survival <- function() {
  loaded_model <- readRDS(file = "models/clinical_logistic_survival.rds")
  return (loaded_model)
}

## Naive Bayes Models
train_clinical_nb_model <- function(df) {
  library(caret)
  library(e1071)
  df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
  df <- subset(df, select = -cohort) # not useful
  df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
  
  ## split data
  trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)
  trainData <- df[trainIndex, ]
  testData <- df[-trainIndex, ]
  
  model <- naiveBayes(death_from_cancer ~ ., data = trainData, laplace=3)
  print(model)
  
  predictions <- predict(model, newdata = testData)
  confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
  print(confMatrix)
  
  save_model("clinical_nb.rds", confMatrix = confMatrix, model=model)
}

get_clinical_nb_model <- function() {
  loaded_model <- readRDS(file = "models/clinical_nb.rds")
  return (loaded_model)
}