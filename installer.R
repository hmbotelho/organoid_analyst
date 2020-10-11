# These are the pacakges that Organoid Analyst depends on
dependencies <- c("shiny", "shinyFiles", "shinyjs", 
                  "ggplot2", "dplyr", "caTools", "raster",
                  "parallel", "fs", "xlsx", "magick")



# Loads R packages (or downloads, installs and loads if the package is not available).
# packages: an array of package names
setupPackages <- function(packages){
    # https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
    
    # load or install&load all
    lapply(packages, function(x) {
        
        print(paste0("--- Loading package '", x, "'"), quote=F)
        
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
        
        # Check that the package is now loaded
        if(paste0("package:", x) %in% search()){
            print(paste0("    Loaded '", x, "' successfully"), quote=F)
        } else{
            stop("Could not load package '", x, "'. Check the package documentation for installation instructions.")
        }
    })
}

setupPackages(dependencies)