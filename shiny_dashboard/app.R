library(shiny)
library(shinydashboard)
source("scripts/overview.R")
source("scripts/clinical_prognosis.R")


ui <- dashboardPage(
  dashboardHeader(title = "Breast-Cancer Dashboard"),
    dashboardSidebar(sidebarMenu(
    menuItem("Data Overview", icon = icon("house"), tabName = "overview"),
    menuItem("Clinical Prognosis",tabName = "prognosis_clinical",icon = icon("person-dress"))
  )),
  
  dashboardBody(tabItems(
    tabItem(tabName = "prognosis_clinical",
      get_clinical_prognosis_ui()
    ),
    tabItem(tabName = "overview",
            get_overview_ui()
  )
  )),
)

server <- function(input, output) {
  get_overview_Server(input,output)
  get_clinical_prognosis_server(input,output)
 
}

shinyApp(ui, server)


