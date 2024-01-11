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
df <- subset(df, select = -nottingham_prognostic_index) # gene data (not in this dataset)
last_col_index <- which(names(df) == "death_from_cancer")
df_x <- df[,0:(last_col_index - 1)]
df_y <- df[,last_col_index]

control <- rfeControl(functions=nbFuncs, method="cv", number=2, verbose = TRUE)
results <- rfe(df_x, df_y, rfeControl=control, sizes = c(1:19))
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
               method = "glmnet",
               trControl = ctrl,
               metric='Accuracy'
)
predictions <- predict(model, testData)
confMatrix <- confusionMatrix(predictions, testData$death_from_cancer)
print(confMatrix)




### CLUSTERING
df <- get_raw_clinical_data(balance_data = FALSE)
#df <- get_raw_gene_data(balance_data = FALSE)
#df <- get_raw_data(balance_data = FALSE)

df <- subset(df, select = -cohort) # not useful
df <- subset(df, select = -integrative_cluster) # gene data (not in this dataset)
numeric <- df %>% select(where(is.numeric))

cl <- kmeans(numeric, 2)
numeric <- numeric %>% mutate(cluster = cl$cluster, death_from_cancer = df$death_from_cancer)

numeric %>% group_by(cluster) %>% summarise(
  survival_months = mean(overall_survival_months),
  tumor_size = mean(tumor_size),
  lymph_nodes_examined_positive = mean(lymph_nodes_examined_positive)
  )

plot(numeric, col = cl$cluster)


