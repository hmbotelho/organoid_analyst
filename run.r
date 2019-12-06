source("functions.r")

dependencies <- c("shiny", "shinyjs", "ggplot2", "tcltk",
                  "caTools", "xlsx", "parallel", "magick",
                  "dplyr", "raster")
sapply(dependencies, loadLibrary)

library(shiny)
runApp(launch.browser = TRUE)
