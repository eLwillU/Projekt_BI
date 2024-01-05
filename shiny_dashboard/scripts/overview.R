source("scripts/clustering_clean.R")
source("scripts/plots_clean.R")



get_overview_ui <- function(){
  return(
    fluidPage(
      h1("Overview"),
      box(plotOutput("plot1")),
      box(plotlyOutput("plot2")),
      box(plotlyOutput("plot3")),
      box(plotlyOutput("plot4")),
      box(plotlyOutput("plot5")),
      box(plotlyOutput("plot6")),
      box(plotOutput("plot8")),
      box(plotOutput("plot9")),
      
)
      
  )
}
## TODO: add plot 7
get_overview_Server <- function(input, output){
  output$plot1 <- renderPlot({get_static_heatmap(4)})
  output$plot2 <- renderPlotly({ get_plotly_heatmap(4)
  })
  output$plot3 <- renderPlotly(get_survival_by_cancertype_plot())
  output$plot4 <- renderPlotly(get_survival_by_cancer_or_disease())
  output$plot5 <- renderPlotly(get_death_from_cancer_with_avg_age())
  output$plot6 <- renderPlotly(get_cohort_pie_chart())
  output$plot7 <- renderPlotly(get_cohort_pie_chart())
  output$plot8 <- renderPlot(get_dfc_dendrogram())
  output$plot9 <- renderPlot(get_not_dfc_dendrogram())
  
  return(output)
}
