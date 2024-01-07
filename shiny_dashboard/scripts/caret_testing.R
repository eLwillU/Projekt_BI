source("scripts/preprocessing.R")
library(caret)
library(e1071)
library(dplyr)
library(plotly)

clinical_data <- get_raw_clinical_data(normalize_data = FALSE)

clinical_data$age_at_diagnosis

get_generic_linechart <- function(data, x, y, title, xlabel, ylabel) {
  fig <-
    plot_ly(data = clinical_data,
            x = ~ x,
            y = ~ y) %>% layout(
              title = title,
              xaxis = list(title = xlabel),
              yaxis = list(title = ylabel)
            )
  
  return(fig)
}

get_generic_linechart(
  clinical_data,
  clinical_data$overall_survival_months,
  clinical_data$lymph_nodes_examined_positive,
  "Impact of positive examined lymphnodes on survival in months",
  "Survival in months",
  "Lymphnodes examined positive"
)

get_generic_linechart(
  clinical_data,
  clinical_data$overall_survival_months,
  clinical_data$mutation_count,
  "Impact of mutations on survival in months",
  "Survival in months",
  "Amount of mutations"
)

get_generic_linechart(
  clinical_data,
  clinical_data$overall_survival_months,
  clinical_data$age_at_diagnosis,
  "Impact of age on survival in months",
  "Survival in months",
  "Age at diagnosis"
)

fig <- plot_ly(data = clinical_data, x = ~overall_survival_months, y = ~lymph_nodes_examined_positive)
fig

fig <- plot_ly(data = clinical_data, x = ~overall_survival_months, y = ~mutation_count)
fig

fig <- plot_ly(data = clinical_data, x = ~overall_survival_months, y = ~age_at_diagnosis)
fig




get_generic_piechart <- function(clinical_data, labels, title, labelType = "label+percent") {
  fig <-
    plot_ly(
      data = clinical_data,
      labels = ~ labels,
      type = 'pie',
      textposition = 'inside',
      textinfo = labelType
    )
  fig <-
    fig %>% layout(
      title = title,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      )
    )
  
  return(fig)
}

get_generic_piechart(clinical_data, clinical_data$death_from_cancer, "test")



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
