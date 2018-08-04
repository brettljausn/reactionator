packages <-
  c("shiny",
    "shinydashboard",
    "rlang",
    "DT",
    "data.table",
    "rhandsontable",
    "ggplot2",
    "reshape2",
    "pracma")

if (!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

p_load(char = packages)

print("Packages installed and loaded successfully")
