packages <-
  c("shiny",
    "shinydashboard",
    "rlang",
    "DT",
    "data.table",
    "rhandsontable")

for (i in packages) {
  if (require(as.name(i), character.only = T)) {
    print(paste(i, "is loaded correctly"))
  } else {
    print(paste("trying to install", i))
    install.packages(i)
    if (require(as.name(i), character.only = T)) {
      print(paste(i, "installed and loaded"))
    } else {
      stop(paste("could not install", i))
    }
  }
}