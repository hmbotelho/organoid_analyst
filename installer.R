# Loads a library. If required, also downloads and installs it.
loadLibrary <- function(libname){
    
    # If not available locally, download and install the library
    if(!(libname %in% installed.packages()[,1])){
        print(paste0("--- Installing package '", libname, "'"))
		install.packages(libname)
		install.packages(libname, type="source")
    }
    
    # If not in memory, load library
    if(!(libname %in% loadedNamespaces())){
        print(paste0("--- Loading package '", libname, "'"), quote=F)
		library(libname, character.only = TRUE)
        
    } else{
        print(paste0("--- Library '", libname, "' is already loaded."), quote=F)
    }
}


dependencies <- c("shiny", "shinyFiles", "shinyjs", 
                  "ggplot2", "dplyr", "caTools", "raster",
                  "parallel", "fs", "xlsx", "magick")

sapply(dependencies, loadLibrary)