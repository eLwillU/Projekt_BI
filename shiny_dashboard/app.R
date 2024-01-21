library(shiny)
library(shinydashboard)
source("dashboards/overview.R")
source("dashboards/clinical_prognosis.R")
source("dashboards/data_viewer.R")


ui <- dashboardPage(
  dashboardHeader(title = "Breast-Cancer Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Data Overview", icon = icon("house"), tabName = "overview"),
    menuItem(
      "Clinical Prognosis",
      tabName = "prognosis_clinical",
      icon = icon("person-dress")
    ),
    menuItem(
      "Data viewer",
      tabName = "data_viewer",
      icon = icon("database")
    )
  )),
  
  dashboardBody(tabItems(
    tabItem(tabName = "prognosis_clinical",
            get_clinical_prognosis_ui()),
    tabItem(tabName = "overview",
            get_overview_ui()),
    tabItem(tabName = "data_viewer",
            get_dataviewer_ui())
  )),
)

server <- function(input, output) {
  get_overview_Server(input, output)
  get_clinical_prognosis_server(input, output)
  get_dataviewer_Server(input, output)
}

shinyApp(ui, server)