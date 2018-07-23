## app.R ##
rm(list = ls())


library(shiny)
library(shinydashboard)
library(rsconnect)
library(DT)
library(data.table)
library(rhandsontable)

source("./scripts/generate_equations.R")

ui <- dashboardPage(
  dashboardHeader(title = "REACTIONATOR",tags$li(class = "dropdown",
                                                 tags$a("v0.2.0", href = "https://github.com/brettljausn/reactionator/"))),
  dashboardSidebar(sidebarMenu(
    menuItem("Simulation",
             tabName = "simulation",
             icon = icon("tint")),
    menuItem(
      "Contact/Feedback",
      tabName = "contact",
      icon = icon("envelope")
    )
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "contact",
            fluidRow(
              box(
                title = "E-Mail",
                uiOutput("email"),
                solidHeader = TRUE,
                status = "primary"
              ),
              box(
                title = "GitHub",
                uiOutput("github"),
                solidHeader = TRUE,
                status = "primary"
              )
            )),
    tabItem(tabName = "simulation",
            fluidRow(
              box(
                title = "Stoichiometric matrix",
                solidHeader = TRUE,
                status = "primary",
                splitLayout(
                  numericInput(
                    "number_species",
                    "Number of species:",
                    3,
                    min = 1,
                    max = 24
                  ),
                  numericInput(
                    "number_reactions",
                    "Number of reactions:",
                    2,
                    min = 1,
                    max = 10
                  )
                ),
                rHandsontableOutput("species_list"),
                htmlOutput("chem_equations")
              ),
              box(
                title = "Concentrations",
                solidHeader = TRUE,
                status = "primary",
                rHandsontableOutput("concentrations")
              ),
              box(
                title = "Reaction rate constants",
                solidHeader = TRUE,
                status = "primary",
                rHandsontableOutput("reaction_rates")
              )
            ))
  ))
)

server <- function(input, output) {
  # create mail-link and output for ui
  email <-
    a("flx.lechleitner@gmail.com", href = "mailto:flx.lechleitner@gmail.com")
  output$email <- renderUI({
    tagList(email)
  })
  
  # create github-link and output for ui
  github <-
    a("https://github.com/brettljausn/reactionator/", href = "https://github.com/brettljausn/reactionator/")
  output$github <-
    renderUI({
      tagList("GitHub-Repository: ", github)
    })
  
  output$species_list <- renderRHandsontable({
    stoic_matrix <- data.table(reaction = 1:input$number_reactions)
    stoic_matrix[, as.character(LETTERS[1:input$number_species])] <-
      as.integer(0)
    rhandsontable(stoic_matrix, rowHeaders = NULL) %>% hot_col("reaction", readOnly = TRUE)
  })
  
  output$concentrations <- renderRHandsontable({
    concentrations <- data.table(species = paste0("c(",colnames(hot_to_r(input$species_list))[2:NCOL(hot_to_r(input$species_list))],")"))
    concentrations$c <- 0
    rhandsontable(concentrations, rowHeaders = NULL, colHeaders = NULL)
  })
  
  output$reaction_rates <- renderRHandsontable({
    rates <- data.table(reaction = paste0("k", 1:input$number_reactions))
    rates$k <- 0
    rhandsontable(rates, rowHeaders = NULL, colHeaders = NULL)
  })
  
  
  output$chem_equations <- renderUI({
    equations <- generate_equations(hot_to_r(input$species_list))
    HTML(paste(equations, collapse = '<br/>'))
  })
  
  
  
  
  
  
  
  
}



shinyApp(ui, server)