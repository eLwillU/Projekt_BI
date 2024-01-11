source("scripts/preprocessing.R")
source("scripts/models.R")
library(caret)
library(e1071)
library(dplyr)
library(plotly)

### FEATURE SELECTION
# clinical
df <- get_raw_clinical_data(balance_data = TRUE)

df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
df <- subset(df, select = -cohort) # not useful
df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
df <- subset(df, select = -nottingham_prognostic_index) # gene data (not in this dataset)
last_col_index <- which(names(df) == "death_from_cancer")
df_x <- df %>% select(-death_from_cancer)
df_y <- df[,last_col_index]

control <- rfeControl(functions=rfFuncs, method="cv", number=2, verbose = TRUE)
results <- rfe(df_x, df_y, rfeControl=control, sizes = c(1:ncol(df_x)))
print(results)
print(results$optVariables)

filtered_df <- df %>% select(results$optVariables) %>% mutate(death_from_cancer = df$death_from_cancer)
trainIndex <- createDataPartition(filtered_df$death_from_cancer, p = .8, list = FALSE, times = 1)
trainData <- filtered_df[trainIndex, ]
testData <- filtered_df[-trainIndex, ]

ctrl = trainControl(method = "cv", number = 2, 
                    verboseIter = TRUE)
model <- train(death_from_cancer ~., 
               data = trainData,
               method = "rf",
               trControl = ctrl,
               metric='Accuracy'
)
predictions <- predict(model, testData)
confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
print(confMatrix)

