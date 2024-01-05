source("scripts/preprocessing.R")
library(ggplot2)
library(ggformula)
library(ggeasy)
library(dplyr)
library(plotly)

data <- load_clinical_data()


get_survival_by_cancertype_plot <- function(){
return(plot_ly(data = data, x=~cancer_type_detailed, y=~overall_survival_months, type="box", color=~cancer_type_detailed,  boxpoints = "all", jitter = 0.3,
        pointpos = -1.8)%>% 
  layout(title = "overall survival months per cancer type",
         xaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff',
                      showticklabels=FALSE,
                      title=list(text='Cancer Types')
         ),
         
         yaxis = list(
           title="Overall Survival Months"
         ),
         legend = list(orientation = 'h',
                       y=-0.3)))
}

get_survival_by_cancer_or_disease <- function() {
return(
  plot_ly(data = data, x=~death_from_cancer,y=~overall_survival_months, type="box") %>%
  layout(title="Overall surivival Months comaring cancer or not",
         xaxis = list(
           title="Death from cancer"
         ),
         yaxis = list(
           title="Overall survival Months"
         )))
}