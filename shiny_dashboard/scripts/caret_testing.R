source("scripts/preprocessing.R")
library(MASS)
library(caret)
library(e1071)
library(dplyr)


raw <- get_raw_data(remove_highly_correlated = TRUE, normalize_data = TRUE)

## get data
df <- get_raw_clinical_data(balance_data = FALSE)
df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
df <- subset(df, select = -cohort) # not useful


## automatic creation with caret train
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
