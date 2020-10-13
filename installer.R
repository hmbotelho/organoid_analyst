# These are the pacakges that Organoid Analyst depends on
dependencies <- c("shiny", "shinyFiles", "shinyjs", 
                  "ggplot2", "dplyr", "caTools", "DT", "raster",
                  "parallel", "fs", "openxlsx", "magick")



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




# caTools
# since version 1.18.0 package 'caTools' requires R >= 3.6.0
# Organoid is compatible with caTools 1.17.1.4 (R >= 2.2.0)
Rver <- sub("^R version (.*?) (.*)$", "\\1", R.version$version.string)

if(utils::compareVersion(Rver, "3.6.0") %in% c(0,1)){
    # Rversion >= 3.6.0
    # Compatible with the latest caTools version
    # Leave the loading process to setupPackages(), called below
} else{
    # R < 3.6.0
    # Not compatible with latest caTools version
    if(utils::compareVersion(Rver, "2.2.0") %in% c(0,1)){
        # 2.2.0 <= Rversion < 3.6.0
        # Install caTools 1.17.1.4
        install.packages("https://cran.r-project.org/src/contrib/bitops_1.0-6.tar.gz", repos=NULL, type="source", dependencies = TRUE)
        install.packages("https://cran.r-project.org/src/contrib/Archive/caTools/caTools_1.17.1.4.tar.gz", repos=NULL, type="source", dependencies = TRUE)
    } else{
        stop("Organoid Analyst is not compatible with R version ", Rver)
    }
}



setupPackages(dependencies)
