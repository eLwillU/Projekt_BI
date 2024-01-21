source("scripts/preprocessing.R")
library(DT)

get_dataviewer_ui <- function() {
  return(fluidPage(
    h1("Data used in this project"),
    p(
      "Shows all the clinical data that was used in this project to train models and create the plots. The genetic data was filtered out for most use-cases as there are too many columns to display in this dashboard."
    ),
    p(
      "The data was preprocessing pipeline to fix various issues. This is the cleaned data without any apparent issues present."
    ),
    fluidRow(column(12,
                    dataTableOutput('table')))
  ))
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
