library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Breast-Cancer Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("house")),
    menuItem("Prognosis",tabName = "prognosis",icon = icon("person-dress"))
  )),
  dashboardBody(tabItems()),
)


server <- function(input, output) {
  
}


shinyApp(ui, server)
