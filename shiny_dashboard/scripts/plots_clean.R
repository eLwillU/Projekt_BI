source("scripts/preprocessing.R")
library(ggplot2)
library(ggformula)
library(ggeasy)
library(dplyr)
library(plotly)

data <- get_raw_clinical_data()

fig <- get_survival_by_cancertype_plot()


get_survival_by_cancertype_plot <- function(){
return(plot_ly(data = data, x=~cancer_type_detailed, y=~overall_survival_months, type="box", color=~cancer_type_detailed,  boxpoints = "all", jitter = 0.3,
        pointpos = -1.8)%>% 
  layout(title = "overall survival months per cancer type",
         xaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff',
                      showticklabels=FALSE,
                      title=list(text='Cancer Types', standoff = 0)
         ),
         
         yaxis = list(
           title="Overall Survival Months"
         ),
         legend = list(orientation = 'h')))
}

