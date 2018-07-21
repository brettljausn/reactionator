## app.R ##
library(shiny)
library(shinydashboard)
library(rsconnect)

ui <- dashboardPage(
  dashboardHeader(title = "REACTIONATOR"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Kontakt/Feedback",
      tabName = "contact",
      icon = icon("envelope")
    )
  )),
  dashboardBody(tabItems(tabItem(
    tabName = "contact",
    fluidRow(box(title = "E-Mail", uiOutput("email")),
             box(title = "GitHub", uiOutput("github")))
  )))
)

server <- function(input, output) {
  # create mail-link and output for ui
  email <-
    a("flx.lechleitner@gmail.com", href = "mailto:flx.lechleitner@gmail.com")
  output$email <- renderUI({
    tagList(email)
  })
  
  # create github-link and output for ui
  github <- a("https://github.com/brettljausn/reactionator/", href = "https://github.com/brettljausn/reactionator/")
  output$github <- renderUI({tagList("GitHub-Repository: ", github)})

  
}

shinyApp(ui, server)
