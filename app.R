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

stoic_matrix <<- data.frame(reaction = 1:2)
stoic_matrix[, as.character(LETTERS[1:3])] <- c(-2,2,-1,1,2,-2)
stoic_matrix <<- stoic_matrix

concentrations <<-
  data.table(species = paste0("c(", colnames(stoic_matrix)[2:NCOL(stoic_matrix)], ")"))
concentrations$c <- c(2,1,0.5)
concentrations <<- concentrations

rates <-
  data.table(reaction = paste0("k", 1:2))
rates$k <- c(0.1,0.01)
rates <<- rates

ui <- dashboardPage(
  dashboardHeader(title = "REACTIONATOR", tags$li(
    class = "dropdown",
    tags$a("v0.5.3", href = "https://github.com/brettljausn/reactionator/")
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
              ),
              box(
                title = "Thanks!",
                textOutput("thanks"),
                solidHeader = TRUE,
                status = "primary"
              )
            )),
    tabItem(tabName = "simulation",
            fluidRow(
              column(
                width = 5,
                box(
                  title = "Stoichiometric matrix",
                  width = NULL,
                  solidHeader = TRUE,
                  status = "primary",
                  splitLayout(
                    # numericInput(
                    #   "number_species",
                    #   "Number of species:",
                    #   3,
                    #   min = 1,
                    #   max = 24
                    # ),
                    sliderInput("number_species", "Number of species:", 1, 24, 3),
                    sliderInput("number_reactions", "Number of reactions:", 1, 10, 2)
                  ),
                  rHandsontableOutput("species_list"),
                  htmlOutput("chem_equations")
                ),column(width = 12,
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
                )),column(width = 12,
                box(title = "Algorithms",
                    solidHeader = TRUE,
                    status = "primary",
                    checkboxGroupInput("algorithm", "Calculation algorithm used:",
                                       c("Explicit Euler" = "euler",
                                         "2nd Order Runge-Kutta" = "rk"),
                                       selected = "euler")),
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
                ))
              ), column(
                width = 7,
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
  init <- "foo"
  
  # create mail-link and output for ui
  email <-
    a("flx.lechleitner@gmail.com", href = "mailto:flx.lechleitner@gmail.com")
  output$email <- renderUI({
    tagList(email)
  })
  
  # credit where credit is due
  output$thanks <- renderText({"Special thanks to Dr. Werner Stadlmayr who inspired me to create this project."})
  
  # create github-link and output for ui
  github <-
    a("https://github.com/brettljausn/reactionator/", href = "https://github.com/brettljausn/reactionator/")
  output$github <-
    renderUI({
      tagList("GitHub-Repository: ", github)
    })
  
  # create stoichiometric matrix
  output$species_list <- renderRHandsontable({
    # add reaction
    if (NROW(stoic_matrix) < input$number_reactions){
      diff <- input$number_reactions - NROW(stoic_matrix)
      to_add <- data.frame(reaction = (NROW(stoic_matrix)+1):(NROW(stoic_matrix)+diff))
      to_add[,names(stoic_matrix)[-1]] <- 0
      stoic_matrix <<- rbind(stoic_matrix, to_add)
    }
    
    # remove reaction
    if (NROW(stoic_matrix) > input$number_reactions){
      stoic_matrix <<- stoic_matrix[1:input$number_reactions,]
      stoic_matrix[, 2:NCOL(stoic_matrix)] <<- sapply(stoic_matrix[, 2:NCOL(stoic_matrix)], as.numeric)
    }
    
    # remove species
    if (NCOL(stoic_matrix) > (input$number_species+1)){
      stoic_matrix <<- stoic_matrix[,1:(input$number_species+1)]
      stoic_matrix[, 2:NCOL(stoic_matrix)] <<- sapply(stoic_matrix[, 2:NCOL(stoic_matrix)], as.numeric)
    }
    
    # add species
    if (NCOL(stoic_matrix) < (input$number_species+1)){
      diff <-  input$number_species - NCOL(stoic_matrix)
      to_add <- data.frame()
      to_add[1:NROW(stoic_matrix),LETTERS[NCOL(stoic_matrix)]] <- 0
      if (diff > 1){
        to_add[LETTERS[NCOL(stoic_matrix):(NCOL(stoic_matrix)+(diff-1))]]
      }
      stoic_matrix <<- cbind(stoic_matrix, to_add)
    }
    
    rhandsontable(stoic_matrix, rowHeaders = NULL) %>% hot_col("reaction", readOnly = TRUE)
  })
  
  observeEvent(input$species_list,{
    stoic_matrix <<- hot_to_r(input$species_list)
  })
  
  observeEvent(input$concentrations,{
    concentrations <<- hot_to_r(input$concentrations)
  })
  
  # generate concentrations table
  output$concentrations <- renderRHandsontable({
    req(input$number_species)
    
    # add concentrations
    if(input$number_species > NROW(concentrations)){
      new_concentrations <- colnames(stoic_matrix)[(NROW(concentrations)+2):NCOL(stoic_matrix)]
      new_concentrations <- paste0("c(", new_concentrations,")")
      to_add <- data.frame(species = new_concentrations, c = 0, stringsAsFactors = F)
      concentrations <<- rbind(concentrations, to_add)
    }
    
    # remove concentrations
    if(input$number_species < NROW(concentrations)){
      concentrations <<- concentrations[1:(NCOL(stoic_matrix)-1)]
    }

    rhandsontable(concentrations,
                  rowHeaders = NULL,
                  colHeaders = NULL,
                  viewportColumnRenderingOffset = 10) %>%  hot_col(1, readOnly = TRUE)
  })
  
  # generate reaction rates table
  output$reaction_rates <- renderRHandsontable({
    req(input$number_reactions)
    
    # remove reaction rates
    if(input$number_reactions < NROW(rates)){
      rates <<- rates[1:input$number_reactions]
    }
    
    # add reaction rates
    if(input$number_reactions > NROW(rates)){
      diff <- input$number_reactions - NROW(rates)
      to_add <-
        data.table(reaction = paste0("k", (NROW(rates)+1):(NROW(rates)+(diff))), k = 0)
      rates <<- rbind(rates,to_add)
    }
    
    rhandsontable(rates, rowHeaders = NULL, colHeaders = NULL, viewportColumnRenderingOffset = 10) %>% hot_col(1, readOnly = TRUE)
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
        input$stepsize,
        input$algorithm
      )
      calc_concentrations(
        hot_to_r(input$concentrations),
        hot_to_r(input$reaction_rates),
        hot_to_r(input$species_list),
        input$endtime,
        input$stepsize,
        input$algorithm
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
          input$stepsize,
          input$algorithm
        )[[2]],
        file
      )
    }
  )
  
  
  
  
  
}



shinyApp(ui, server)
