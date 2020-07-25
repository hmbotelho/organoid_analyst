source("functions.r")

dependencies <- c("shiny", "shinyFiles", "shinyjs", 
                  "ggplot2", "dplyr", "caTools", "raster",
                  "parallel", "fs", "xlsx", "magick")
sapply(dependencies, loadLibrary)

runApp(launch.browser = TRUE)
