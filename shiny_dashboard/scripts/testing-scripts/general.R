source("scripts/models.R")
source("scripts/preprocessing.R")
library(rpart)
library(rpart.plot)

data <- get_raw_clinical_data()

## logistic plotting
model3 <- get_clinical_dectree_model_survival()
printcp(model3$model)



get_dectree_plots <- function(tree) {
  library(rpart)
  library(rpart.plot)
  
  plot1 <- plotcp(tree)
  pruned_tree <- prune(tree, cp=0.0050623)
  plot2 <- prp(pruned_tree, roundint=FALSE)
  
  return (c(plot1, plot2))
}

huh <- get_dectree_plots(model3$model)

print(huh[1])
