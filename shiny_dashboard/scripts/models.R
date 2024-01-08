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
  
  # split data
  trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)
  trainData <- df[trainIndex, ]
  testData <- df[-trainIndex, ]
  # make model
  model <- glm(death_from_cancer ~ ., data = trainData, family = 'binomial')
  model_stepped <- step(model, trace = FALSE, direction= "both")
  print(summary(model_stepped))
  print(model_stepped)
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
train_clinical_nb_model_survival <- function(df) {
  # TODO: feature selection?
  library(caret)
  library(e1071)
  df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
  df <- subset(df, select = -cohort) # not useful
  df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
  
  # split data
  trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)
  trainData <- df[trainIndex, ]
  testData <- df[-trainIndex, ]
  
  # make and validate model
  model <- naiveBayes(death_from_cancer ~ ., data = trainData, laplace=3)
  print(model)
  predictions <- predict(model, newdata = testData)
  confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
  print(confMatrix)
  
  save_model("clinical_nb_survival.rds", confMatrix = confMatrix, model=model)
}

get_clinical_nb_model_survival <- function() {
  library(e1071)
  loaded_model <- readRDS(file = "models/clinical_nb_survival.rds")
  return (loaded_model)
}

## Trees
train_clinical_dectree_model_survival <- function(df, cp=0.001) {
  # TODO: feature selection?
  # probably not https://topepo.github.io/caret/feature-selection-overview.html
  library(rpart)
  library(caret)
  df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
  df <- subset(df, select = -cohort) # not useful
  df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
  
  df <- subset(df, select = -cellularity) # removed because of bug I cant explain
  df <- subset(df, select = -neoplasm_histologic_grade) # removed because of bug I cant explain
  
  # split data
  trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)
  trainData <- df[trainIndex, ]
  testData <- df[-trainIndex, ]
  
  # make and plot model
  model <- rpart(death_from_cancer~., method="class", data=trainData, cp=cp)
  printcp(model) 
  plotcp(model)
  plot(model, uniform=TRUE, main="Classification Tree for Clinical Breastcancer Data")
  text(model, use.n=TRUE, all=TRUE, cex=.8)
  
  # validate model
  predictions <- predict(model, testData, type = "class")
  confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
  print(confMatrix)
  
  save_model("clinical_dectree_survival.rds", confMatrix = confMatrix, model=model)
}

get_clinical_dectree_model_survival <- function() {
  # TODO: feature selection?
  # probably not https://topepo.github.io/caret/feature-selection-overview.html
  library(rpart)
  loaded_model <- readRDS(file = "models/clinical_dectree_survival.rds")
  return (loaded_model)
}

train_clinical_rftree_model_survival <- function(df) {
  library(randomForest)
  library(caret)
  df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
  df <- subset(df, select = -cohort) # not useful
  df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
  
  # split data
  trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)
  trainData <- df[trainIndex, ]
  testData <- df[-trainIndex, ]
  
  ctrl = trainControl(method = "cv", number = 10, 
                      verboseIter = TRUE)
  
  # make and plot model
  model <- train(death_from_cancer ~., 
                 data = trainData,
                 method = "rf",
                 trControl = ctrl,
                 metric='Accuracy'
  )
  
  # validate model
  predictions <- predict(model, testData)
  confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
  print(confMatrix)
  
  save_model("clinical_rftree_survival.rds", confMatrix = confMatrix, model=model)
}

get_clinical_rftree_model_survival <- function() {
  library(randomForest)
  loaded_model <- readRDS(file = "models/clinical_rftree_survival.rds")
  return (loaded_model)
}

# Create a random forest model with the gene data set.
train_gene_rf_model <- function(df){
  library(randomForest)
  randomForest_model <- randomForest(death_from_cancer ~ ., data = genomic_data)
  saveRDS(randomForest_model,"models/gene_rf_death_from_cancer.rds")
}
# Load the random forest model with the gene data set.
get_gene_rf_model <- function(){
  library(randomForest)
  loaded_model <- readRDS("models/gene_rf_death_from_cancer.rds")
  return(loaded_model)
}


