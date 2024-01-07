library(ggplot2)
library(ggformula)
library(ggeasy)
library(dplyr)
library(plotly)

get_survival_by_cancertype_plot <- function(clinical_data){
    return(plot_ly(data = clinical_data, x=~cancer_type_detailed, y=~overall_survival_months, type="box", color=~cancer_type_detailed) %>% 
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

get_death_from_cancer_with_avg_age <- function(clinical_data){
  dfc_yes<- clinical_data %>%
    filter(death_from_cancer == "yes")
  dfc_no <- clinical_data %>%
    filter(death_from_cancer == "no")
  return(
  plot_ly(data= clinical_data, x=~death_from_cancer ,y=~mean(age_at_diagnosis), type="scatter", mode="lines", name="Avg. Age at Diagnosis") %>%
  add_boxplot(data= dfc_yes,x = ~death_from_cancer, y=~overall_survival_months, inherit  = F, name="Death From Cancer")%>%
  add_boxplot(data= dfc_no,x = ~death_from_cancer, y=~overall_survival_months, inherit  = F, name="Not Death From Cancer") %>%
  layout(title="Death from Cancer vs. no Death from cancer",
         yaxis = list(
           title="Overall survival Months"),
         xaxis = list(
           title="Death from cancer"
         ),
         legend = list(
            orientation="h",
            y=-0.2))
)
}

get_generic_boxplot <-
  function(clinical_data,
           x,
           y,
           title,
           xaxis,
           yaxis,
           colors) {
    fig <-
      plot_ly(
        data = clinical_data,
        x = ~ x,
        y = ~ y,
        type = 'box'
      )
    fig <-
      fig %>% layout(
        title = title,
        xaxis = list(title = xaxis),
        yaxis = list(title = yaxis)
      )
    
    return(fig)
  }

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

get_tumor_size_plot <- function(clinical_data){
  return(
    plot_ly(data = clinical_data, type="box", x=~death_from_cancer, y=~tumor_size )
    
  )
}