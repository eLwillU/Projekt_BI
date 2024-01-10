source("scripts/preprocessing.R")
source("scripts/plots.R")
source("scripts/models.R")
library(caret)
library(e1071)
library(dplyr)
library(plotly)

### FEATURE SELECTION
# clinical
df <- get_raw_clinical_data(balance_data = FALSE)

df <- subset(df, select = -overall_survival_months) # same as death_from_cancer
df <- subset(df, select = -cohort) # not useful
df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
last_col_index <- which(names(df) == "death_from_cancer")
df_x <- df[,0:(last_col_index - 1)]
df_y <- df[,last_col_index]

control <- rfeControl(functions=rfFuncs, method="cv", number=2, verbose = TRUE)
# run the RFE algorithm
results <- rfe(df_x, df_y, rfeControl=control, sizes = c(1:19))
# summarize the results
print(results)


# gene
df <- get_raw_gene_data(balance_data = FALSE)
last_col_index <- which(names(df) == "death_from_cancer")
df_x <- df[,(last_col_index + 1 ): ncol(df)]
df_y <- df[,1:last_col_index]

control <- rfeControl(functions=rfFuncs, method="cv", number = 2, verbose = TRUE)
# run the RFE algorithm
results <- rfe(df_x, df_y, rfeControl=control, sizes = c(1:489))
# summarize the results
print(results)





### IDK WHAT THIS IS
df <- get_raw_clinical_data(balance_data = FALSE)
df <- get_raw_gene_data(balance_data = FALSE)
df <- get_raw_data(balance_data = FALSE)

max(df$nottingham_prognostic_index)
min(df$nottingham_prognostic_index)

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
               )

# model <- train(death_from_cancer ~ ., data = trainData, method = "naive_bayes")
summary(model)

predictions <- predict(model, testData)
confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
print(confMatrix)

