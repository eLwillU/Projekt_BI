source("scripts/preprocessing.R")
source("scripts/plots.R")
source("scripts/models.R")
library(caret)
library(e1071)
library(dplyr)
library(plotly)

df <- get_raw_data(balance_data = FALSE)
df <- get_raw_clinical_data(balance_data = FALSE)
df <- get_raw_gene_data(balance_data = FALSE)

#TODO: discuss how to handle outliers. If removed only 6 rows remain :D
remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}
df <- df %>% 
  mutate(across(where(is.numeric), remove_outliers))
df <- na.omit(df)

train_clinical_rftree_model_survival(df)


## automatic creation with caret train
df <- get_raw_clinical_data(balance_data = FALSE)
df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
df <- subset(df, select = -cohort) # not useful
df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
trainIndex <- createDataPartition(df$death_from_cancer, p = .8, list = FALSE, times = 1)

trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

#Create model

ctrl = trainControl(method = "cv", number = 10, 
                    verboseIter = TRUE)

model <- train(death_from_cancer ~., 
               data = trainData,
               method = "rf",
               trControl = ctrl,
               metric='Accuracy'
               )

# model <- train(death_from_cancer ~ ., data = trainData, method = "naive_bayes")
summary(model)

predictions <- predict(model, testData)
confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
print(confMatrix)

