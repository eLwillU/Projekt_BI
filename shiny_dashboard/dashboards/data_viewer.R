source("scripts/preprocessing.R")
library(DT)

get_dataviewer_ui <- function() {
  return(fluidPage(h1("Data used in this project"),
                   fluidRow(column(
                     12,
                     dataTableOutput('table')
                   ))))
}

get_dataviewer_Server <- function(input, output) {
  clinical_data <- loadData("raw_clinical_data_unbalanced.RDS")
  output$table <-
    renderDataTable(
      clinical_data,
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('colvis')
      )
    )
  
  return(output)
}
