source("scripts/clustering_clean.R")
source("scripts/plots_clean.R")



get_overview_ui <- function(){
  return(
    fluidPage(
      h1("Overview"),
      box(plotOutput("plot1")),
      box(plotlyOutput("plot2")),
      box(plotlyOutput("plot3")),
      box(plotlyOutput("plot4"))
    )
  )
}

get_overview_Server <- function(input, output){
  output$plot1 <- renderPlot({get_static_heatmap(4)})
  output$plot2 <- renderPlotly({ get_plotly_heatmap(4)
  })
  output$plot3 <- renderPlotly(get_survival_by_cancertype_plot())
  output$plot4 <- renderPlotly(get_survival_by_cancer_or_disease())
  return(output)
}
