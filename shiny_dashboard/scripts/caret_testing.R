source("scripts/preprocessing.R")
source("scripts/plots.R")
library(caret)
library(e1071)
library(dplyr)
library(plotly)

clinical_data <- get_raw_clinical_data(normalize_data = FALSE)

numeric_df <- clinical_data %>% select_if(is.numeric)

get_generic_qqplot(numeric_df, numeric_df$age_at_diagnosis, "skajhd")

numeric_df$age_at_diagnosis
numeric_df$lymph_nodes_examined_positive
numeric_df$mutation_count
numeric_df$nottingham_prognostic_index
numeric_df$overall_survival_months
numeric_df$tumor_size

## get data
df <- get_raw_clinical_data(balance_data = FALSE)
df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
df <- subset(df, select = -cohort) # not useful
df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)

## automatic creation with caret train
df <- get_raw_clinical_data(balance_data = FALSE)
trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

model <- train(death_from_cancer ~ ., data = trainData, method = "naive_bayes")
summary(model)

predictions <- predict(model, testData)
confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
print(confMatrix)


## manual logistic regression
trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

model <- glm(death_from_cancer ~ ., data = trainData, family = 'binomial')
model_stepped <- step(model, trace = FALSE, direction= "both")

probabilities <- predict(model_stepped, newdata = testData, type = 'response')
predicted_classes <- ifelse(probabilities > 0.5, "yes", "no")
predicted_classes <- factor(predicted_classes, levels = c("no", "yes"))
confMatrix <- confusionMatrix(predicted_classes, testData$death_from_cancer)
print(confMatrix)

## manual NaiveBayes
trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]
nb_model <- naiveBayes(death_from_cancer ~ ., data=trainData)
predictions <- predict(nb_model, testData)
confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
confMatrix
