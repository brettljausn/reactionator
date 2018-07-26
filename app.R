## app.R ##
rm(list = ls())

# comment the following source command when deploying
# source("./scripts/install_packages.R")

# comment the library commands when launching locally
library(shiny)
library(shinydashboard)
library(rsconnect)
library(DT)
library(data.table)
library(rhandsontable)
library(rlang)
library(ggplot2)
library(reshape2)

source("./scripts/generate_equations.R")
source("./scripts/calculate_concentration_profile.R")


ui <- dashboardPage(
  dashboardHeader(title = "REACTIONATOR", tags$li(
    class = "dropdown",
    tags$a("v0.4.1", href = "https://github.com/brettljausn/reactionator/")
  )),
  dashboardSidebar(sidebarMenu(
    menuItem("Simulation",
             tabName = "simulation",
             icon = icon("flask")),
    menuItem(
      "Thermodynamic data",
      tabName = "thermo",
      icon = icon("calculator")
    ),
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
              column(
                width = 4,
                box(
                  title = "Stoichiometric matrix",
                  width = NULL,
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
                ),
                box(
                  title = "Time",
                  solidHeader = TRUE,
                  status = "primary",
                  splitLayout(
                    numericInput("endtime",
                                 "Time",
                                 10),
                    numericInput("stepsize",
                                 "stepsize:",
                                 0.1)
                  )
                )
              ), column(
                width = 8,
                box(
                  title = "Results",
                  solidHeader = TRUE,
                  width = NULL,
                  status = "primary",
                  plotOutput("result_plot"),
                  downloadButton("download_data", label = "Download data (.csv)")
                )
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
      as.numeric(0)
    rhandsontable(stoic_matrix, rowHeaders = NULL) %>% hot_col("reaction", readOnly = TRUE)
  })
  
  # generate concentrations table
  output$concentrations <- renderRHandsontable({
    req(input$species_list)
    concentrations <-
      data.table(species = paste0("c(", colnames(hot_to_r(
        input$species_list
      ))[2:NCOL(hot_to_r(input$species_list))], ")"))
    concentrations$c <- 0
    rhandsontable(concentrations,
                  rowHeaders = NULL,
                  colHeaders = NULL)
  })
  
  # generate reaction rates table
  output$reaction_rates <- renderRHandsontable({
    req(input$number_reactions)
    rates <-
      data.table(reaction = paste0("k", 1:input$number_reactions))
    rates$k <- 0
    rhandsontable(rates, rowHeaders = NULL, colHeaders = NULL)
  })
  
  # parse stoichiometric matrix into equations
  output$chem_equations <- renderUI({
    req(input$species_list)
    equations <- generate_equations(hot_to_r(input$species_list))
    HTML(paste(equations, collapse = '<br/>'))
  })
  
  # calculate and plot data
  output$result_plot <-
    renderPlot({
      req(
        input$reaction_rates,
        input$concentrations,
        input$reaction_rates,
        input$endtime,
        input$stepsize
      )
      calc_concentrations(
        hot_to_r(input$concentrations),
        hot_to_r(input$reaction_rates),
        hot_to_r(input$species_list),
        input$endtime,
        input$stepsize
      )[[1]]
    })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(
        calc_concentrations(
          hot_to_r(input$concentrations),
          hot_to_r(input$reaction_rates),
          hot_to_r(input$species_list),
          input$endtime,
          input$stepsize
        )[[2]],
        file
      )
    }
  )
  
  
  
  
  
}



shinyApp(ui, server)
