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



# Concatenates all csv files matching the pattern. Supports multi-core processing
concatenate.csv <- function(folder = getwd(), pattern = "objects\\.csv", recursive = TRUE, parallelize = TRUE){
    
    # Get file list
    file_list <- list.files(folder, pattern, full.names = TRUE, recursive = recursive)
    
    if(detectCores() <= 2 | !parallelize){
        # Read all files using single core processing
        
        output <- lapply(file_list, function(file){
            message(paste0("    ", file))
            read.csv(file, stringsAsFactors = FALSE)
        })
        
    } else{
        # Read all files using multi-core processing
        
        cluster <- makePSOCKcluster(detectCores() - 1)
        output <- parLapply(cluster, file_list, function(file){
            read.csv(file, stringsAsFactors = FALSE)
        })
        stopCluster(cluster)
    }

    # Return concatenated data.frame
    do.call("rbind", output)
}




# Segmentation masks ------------------------------------------------------
# dataset <- dget("c:/users/hugo/Desktop/dataset.rdata")
# df                      <- dataset
# colPath_oldmasks        <- "OA_path_rawmask"
# colPath_newmasks        <- "OA_path_OAmask"
# colPath_newlabels       <- "OA_path_OAlabel"
# colX                    <- "AreaShape_Center_X"
# colY                    <- "AreaShape_Center_Y"
# colLabel                <- "TrackObjects_Label_2"
# discardTheseLabels      <- "NaN"

# Creates a new version of CellProfiler's segmentation mask images that does not show organoids which do not meet QC parameters (as reported by their labels)
makeSegmentationMasks <- function(df, colPath_oldmasks = "analyst_path_CPmasks", colPath_newmasks = "analyst_path_analystmasks", colPath_newlabels = "analyst_path_analystlabels", colX = "AreaShape_Center_X", colY = "AreaShape_Center_Y", colLabel = "TrackObjects_Label_2", discardTheseLabels = "Allow all organoids"){
    
    # library("magick")
    # library("parallel")
    
    img_list <- split(df, df[[colPath_oldmasks]])
    
    
    # Use parallel processing, if possible
    if(detectCores() < 2){
        
        # Use single core processing
        
        lapply(img_list, function(img_df){
            # img_df <- img_list[[1]]

            # Try to deal with existing and missing images
            if(!file.exists(img_df[1, colPath_oldmasks])){

                # Image file does not exist. Create placeholder images.

                message(paste("File not found: ", img_df[1, colPath_oldmasks], ". Creating placeholder blank images."))
                img_masks  <- image_read("./black.png")
                img_labels <- image_read("./black.png")
                
                
            } else{
                
                
                # Image file exists. Process it normally
                
                # Open and initialize images for masks and labels
                img_masks  <- image_read(img_df[1, colPath_oldmasks])
                xres       <- image_info(img_masks)[1,"width"]
                yres       <- image_info(img_masks)[1,"height"]
                img_masks  <- image_convert(img_masks, type ='grayscale')
                img_labels <- image_read("./black.png")
                img_labels <- image_resize(img_labels, paste0(xres,"x",yres))
                
                
                # Go through all organoids in the image:
                #   - Paint excluded organoids black in the masks image
                #   - Create a new image with organoid labels painted on it
                processed_labels <- c()
                for(i in 1: nrow(img_df)){
                    
                    
                    
                    # Get organoid features
                    label <- as.character(img_df[i,colLabel])
                    x     <- img_df[i, colX]
                    y     <- img_df[i, colY]
                    
                    
                    
                    # Make sure no label is processed multiple times (this avoids that an organoid is labeled multiple times)
                    if(label %in% processed_labels){
                        # This organoid has already been processed.
                        # Do nothing
                    } else{
                        # Do not skip this organoid. Just keep a record that this organoid label has been processed
                        processed_labels <- c(processed_labels, label)
                        
                        if("Allow all organoids" %in% discardTheseLabels){
                            # Do not discard any organoids
                            xy         <- paste0("+", x-pixelsWide(label)*0.5, "+", y-pixelsTall(label))
                            img_labels <- image_annotate(img_labels, label, size = 10, color = "white", location = xy)
                        } else if(label %in% discardTheseLabels){
                            # Discard 'bad' organoids
                            xy         <- paste0("+", x, "+", y)
                            img_masks  <- image_fill(img_masks, "black", point = xy, fuzz = 0)
                        } else{
                            # Allowed organoid
                            xy         <- paste0("+", x-pixelsWide(label)*0.5, "+", y-pixelsTall(label))
                            img_labels <- image_annotate(img_labels, label, size = 10, color = "white", location = xy)
                        }   
                    }
                    
                }
                
            }
            
            
            
            
            
            # save images
            if(!dir.exists(dirname(img_df[1,colPath_newmasks]))){
                dir.create(dirname(img_df[1,colPath_newmasks]), recursive = TRUE)
            }
            if(!dir.exists(dirname(img_df[1,colPath_newlabels]))){
                dir.create(dirname(img_df[1,colPath_newlabels]), recursive = TRUE)
            }
            image_write(img_masks,  img_df[1,colPath_newmasks])
            image_write(img_labels, img_df[1,colPath_newlabels])
            message(paste("Saved mask file: ", img_df[1,colPath_newmasks]))
            message(paste("Saved labels file: ", img_df[1,colPath_newlabels]))
            
        })
        
    } else{
        
        # Use parallel processing
        
        cluster  <- makePSOCKcluster(detectCores() - 1)
        clusterExport(cluster, varlist=c("colPath_oldmasks", "colPath_newlabels", "colPath_newmasks", "colX", "colY", "colLabel", "discardTheseLabels", "image_read", "image_graph", "image_info", "image_convert", "image_fill", "image_annotate", "image_write", "image_resize", "blank_magick", "pixelsWide", "pixelsTall"), envir=environment())
        
        parLapply(cluster, img_list, function(img_df){
            # img_df <- img_list[[1]]
            
            # Try to deal with existing and missing images
            if(!file.exists(img_df[1, colPath_oldmasks])){
                
                # Image file does not exist. Create placeholder images.
                
                message(paste("File not found: ", img_df[1, colPath_oldmasks], ". Creating placeholder blank images."))
                img_masks  <- image_read("./black.png")
                img_labels <- image_read("./black.png")
                
                
            } else{
                
                
                # Image file exists. Process it normally
                
                # Open and initialize images for masks and labels
                img_masks  <- image_read(img_df[1, colPath_oldmasks])
                xres       <- image_info(img_masks)[1,"width"]
                yres       <- image_info(img_masks)[1,"height"]
                img_masks  <- image_convert(img_masks, type ='grayscale')
                img_labels <- image_read("./black.png")
                img_labels <- image_resize(img_labels, paste0(xres,"x",yres))
                
                
                # Go through all organoids in the image:
                #   - Paint excluded organoids black in the masks image
                #   - Create a new image with organoid labels painted on it
                processed_labels <- c()
                for(i in 1: nrow(img_df)){
                    
                    
                    
                    # Get organoid features
                    label <- as.character(img_df[i,colLabel])
                    x     <- img_df[i, colX]
                    y     <- img_df[i, colY]
                    
                    
                    
                    # Make sure no label is processed multiple times (this avoids that an organoid is labeled multiple times)
                    if(label %in% processed_labels){
                        # This organoid has already been processed.
                        # Do nothing
                    } else{
                        # Do not skip this organoid. Just keep a record that this organoid label has been processed
                        processed_labels <- c(processed_labels, label)
                        
                        if("Allow all organoids" %in% discardTheseLabels){
                            # Do not discard any organoids
                            xy         <- paste0("+", x-pixelsWide(label)*0.5, "+", y-pixelsTall(label))
                            img_labels <- image_annotate(img_labels, label, size = 10, color = "white", location = xy)
                        } else if(label %in% discardTheseLabels){
                            # Discard 'bad' organoids
                            xy         <- paste0("+", x, "+", y)
                            img_masks  <- image_fill(img_masks, "black", point = xy, fuzz = 0)
                        } else{
                            # Allowed organoid
                            xy         <- paste0("+", x-pixelsWide(label)*0.5, "+", y-pixelsTall(label))
                            img_labels <- image_annotate(img_labels, label, size = 10, color = "white", location = xy)
                        }   
                    }
                    
                }
                
            }
            
            
            
            
            
            # save images
            if(!dir.exists(dirname(img_df[1,colPath_newmasks]))){
                dir.create(dirname(img_df[1,colPath_newmasks]), recursive = TRUE)
            }
            if(!dir.exists(dirname(img_df[1,colPath_newlabels]))){
                dir.create(dirname(img_df[1,colPath_newlabels]), recursive = TRUE)
            }
            image_write(img_masks,  img_df[1,colPath_newmasks])
            image_write(img_labels, img_df[1,colPath_newlabels])
            message(paste("Saved mask file: ", img_df[1,colPath_newmasks]))
            message(paste("Saved labels file: ", img_df[1,colPath_newlabels]))
            
        })
        
        stopCluster(cluster)
        
    }
    
}




#############################################
#  Plotting functions
#############################################
# df           <- read.csv("c:/Users/Hugo/Documents/shiny/organoid_analyst/resultsFisfinal.csv", stringsAsFactors = FALSE)
# slope_xmin   <- 10
# slope_xmax   <- 30
# colX         <- "time"
# colY         <- "normalized"
# colWell      <- "well"
# colSlope     <- "initialSlope"
# numrows      <- 8
# numcols      <- 12
# xmin         <- 0
# xmax         <- 60
# ymin         <- 94.46051
# ymax         <- 220.2863
# title        <- "c:\\Hugo\\organoids\\dataset"
# xlab         <- "Time (min)"
# ylab         <- "Normalized area (%)"
# colHighlight <- "compound"
# showFit      <- TRUE
# showFitLUT   <- TRUE
# labelsize    <- 3
plot_plate <- function(df, slope_xmin, slope_xmax, colX, colY, colWell, colSlope, numrows, numcols, xmin, xmax, ymin, ymax, title, xlab, ylab, colHighlight = "None", showFit = TRUE, showFitLUT = TRUE, labelsize = 3){

    # Make the function work even when arguments have silly values
    if(is.null(showFit))               stop("plot_plate: 'showFit' argument is null")
    if(is.null(showFitLUT))            stop("plot_plate: 'showFitLUT' argument is null")
    if(is.null(colHighlight))          stop("plot_plate: 'colHighlight' argument is null")
    if(!(colHighlight %in% names(df))) colHighlight <- "None"
    
    # Make the function work even if there is no slope data
    if(missing(colSlope)) colSlope <- "initialSlope"
    if(!colSlope %in% names(df)) df[[colSlope]] <- NA


    # Plot limits
    Xmin <- min(df[[colX]], na.rm = TRUE)
    Xmax <- max(df[[colX]], na.rm = TRUE)
    Ymin <- min(df[[colY]], na.rm = TRUE)
    Ymax <- max(df[[colY]], na.rm = TRUE)
    
    
    # Subset data for fitting
    df_fitting <- df[df[[colX]] >= slope_xmin & df[[colX]] <= slope_xmax & !is.na(df[[colY]]),]
    
    
    
    # Initialize plot
    g <- ggplot(df, aes_string(x = colX, y = colY)) +
         geom_point(size = 2) + 
         geom_line(size = 0.5) +
         facet_wrap(as.formula(paste("~", colWell)), ncol = numcols, nrow = numrows) + 
         labs(title = title) +
         labs(x = xlab, y = ylab) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
         theme(axis.text=element_text(size = 10)) + 
         theme(axis.title=element_text(size = 20, face = "bold")) + 
         theme(strip.text.x = element_text(margin = margin(0,0,0,0, "cm")))
         
    
    
    # Color traces by initial swelling rate
    if(showFitLUT){
        g <- g + aes_string(color = colSlope) +
                 scale_colour_gradientn(colours=c("red2", "darkgoldenrod1", "darkgreen"))
    }
    
    
    # Linear fits (initial swelling rate)
    if(showFit){
        g <- g + stat_smooth(data = df_fitting, aes_string(x = colX, y = colY), method = lm, level = FALSE, fullrange = TRUE, size = 1, color = "black")
    }
    
    
    # Label wells according to their contents
    if(colHighlight == "None"){
        
        # Do not label wells
        g <- g + ylim(Ymin, Ymax)
    
    } else{
    
        # Color & label wells
        df$x1   <- Xmin
        df$x2   <- Xmax
        df$xavg <- 0.5 * (Xmin + Xmax)
        df$y1   <- Ymax * 1.10
        df$y2   <- Ymax * 1.25
        df$yavg <- 0.5 * (Ymax * 1.10 + Ymax * 1.25)
        
        g <- g + geom_label(data = df, mapping= aes_string(x = "xavg", y = "yavg", label = colHighlight, fill = colHighlight), colour = "white", size = labelsize)
        
    }
    
    
    # Return finished plot
    g
}



# df                <- read.csv("c:/users/hugo/Documents/shiny/organoid_analyst/resultsFIS_summarycumulativeAUC.csv", stringsAsFactors = FALSE)
# colX              <- "concentration"
# colY              <- "mean"
# colsdY            <- "sd"
# colTreatment      <- "compound"
# pickThisTreatment <- "fsk"
# xlab              <- "x"
# ylab              <- "y"
plot_titration <- function(df, colX, colY, colsdY = "None", colTreatment = "None", pickThisTreatment = "All", categoricalX = TRUE, xlab = "x", ylab = "y", axistitlesize = 20, axislabelsize = 15){

    # Make sure all arguments make sense
    if(!(colX %in% names(df))) stop(paste0("Plot titration: Data frame does not have this 'colX' column: ", colX))
    if(!(colY %in% names(df))) stop(paste0("Plot titration: Data frame does not have this 'colY' column: ", colY))
    
    if(missing(colsdY)) colsdY <- "None"
    if(missing(colTreatment)){
        colTreatment      <- "None"
        pickThisTreatment <- "All"
    }
    if(missing(pickThisTreatment)) pickThisTreatment <- "All"
    
    if(!(colsdY %in% names(df)) | colsdY == "None"){
        warning("Plot titration: Y error bars will not be shown.")
        colsdY <- "sd"
        df[[colsdY]] <- NA
    }
    if(!(colTreatment %in% names(df)) | colTreatment == "None"){
        # Cannot subset based on treatment column
        colTreatment       <- "None"
        pickThisTreatment  <- "All"
        df[[colTreatment]] <- "None"
    }
    if(colTreatment != "None"){
        if(!(pickThisTreatment %in% df[[colTreatment]])){
            pickThisTreatment <- "All"
        }
    }
    
    

    # Subset data frame
    if(!("All" %in% pickThisTreatment)){
        df <- df[df[[colTreatment]] %in% pickThisTreatment,]
    }
    
    

    # Force x axis to continuous or categorical
    if(categoricalX){
        df[[colX]] <- as.factor(df[[colX]])
    } else{
        df[[colX]] <- as.numeric(df[[colX]])
    }
    
    
    
    # Make plot
    limits <- aes(ymax = df[[colY]] + df[[colsdY]], ymin = df[[colY]] - df[[colsdY]])
    
    g <- ggplot(df, aes_string(x = colX, y = colY, colour = colTreatment, group = colTreatment)) + 
                geom_line(size = 1) + 
                geom_errorbar(limits, width = 0.1, size = 1) + 
                labs(x = xlab, y = ylab) +
                theme(axis.title=element_text(size = 20, face = "bold")) +
                theme(axis.text=element_text(size = 15)) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                geom_hline(yintercept = 0, size = 0.5, alpha = 0.5, linetype = "solid")
     
    g
}



# df                <- read.csv("c:/users/hugo/Documents/shiny/organoid_analyst/resultsFIS_summarycumulativeAUC.csv", stringsAsFactors = FALSE)
# colX              <- "treatment"
# colY              <- "mean"
# colsdY            <- "sd"
# title             <- "My title"
# xlab              <- NULL
# ylab              <- "AUC (mean ± sd)"
# axistitlesize     <- 20
# axislabelsize     <- 12
# colTreatment      <- "compound"
# pickThisTreatment <- "All"
# colorbyTreatment  <- TRUE
plot_AUC <- function(df, colX, colY, colsdY, title = NULL, xlab = NULL, ylab = "y", axistitlesize = 20, axislabelsize = 12, colTreatment = "None", pickThisTreatment = "All", colorbyTreatment = FALSE){

    # Subset data frame
    if(!("All" %in% pickThisTreatment)){
        df <- df[df[[colTreatment]] %in% pickThisTreatment,]
    }
    
    limits <- aes(ymax = df[[colY]] + df[[colsdY]], ymin = df[[colY]] - df[[colsdY]])

    g <- ggplot(df, aes_string(x = colX, y = colY)) +
         geom_bar(stat="identity") +
         geom_errorbar(limits, width = 0.2, size = 0.5) + 
         labs(title = title) +
         labs(x = xlab, y = ylab) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
         theme(axis.text=element_text(size = axislabelsize)) + 
         theme(axis.title=element_text(size = axistitlesize, face = "bold"))
    
    # Color bars
    if(colorbyTreatment){
        g <- g + aes_string(fill = colTreatment)
    }
    
    g
}



# df                <- read.csv("c:/users/hugo/Documents/shiny/organoid_analyst/resultsFIS_summarySlope.csv", stringsAsFactors = FALSE)
# colX              <- "treatment"
# colY              <- "mean"
# colsdY            <- "sd"
# title             <- "My title"
# xlab              <- NULL
# ylab              <- "y"
# axistitlesize     <- 20
# axislabelsize     <- 12
# colTreatment      <- "compound"
# pickThisTreatment <- "All"
# colorbyTreatment  <- TRUE
plot_slopes <- function(df, colX, colY, colsdY, title = NULL, xlab = NULL, ylab = "y", axistitlesize = 20, axislabelsize = 12, colTreatment = "None", pickThisTreatment = "All", colorbyTreatment = FALSE){

    # Subset data frame
    if(!("All" %in% pickThisTreatment)){
        df <- df[df[[colTreatment]] %in% pickThisTreatment,]
    }
    
    limits <- aes(ymax = df[[colY]] + df[[colsdY]], ymin = df[[colY]] - df[[colsdY]])
 
    g <- ggplot(df, aes_string(x = colX, y = colY)) +
         geom_bar(stat="identity") +
         geom_errorbar(limits, width = 0.2, size = 0.5) + 
         labs(title = title) +
         labs(x = xlab, y = ylab) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
         theme(axis.text=element_text(size = axislabelsize)) + 
         theme(axis.title=element_text(size = axistitlesize, face = "bold"))
    
    # Color bars
    if(colorbyTreatment){
        g <- g + aes_string(fill = colTreatment)
    }
    
    g
}





# df                <- read.csv("c:/users/hugo/Documents/shiny/organoid_analyst/resultsFIS_summaryNormalized.csv", stringsAsFactors = FALSE)
# colX              <- "treatment"
# colY              <- "mean"
# colsdY            <- "sd"
# title             <- "My title"
# xlab              <- NULL
# ylab              <- "y"
# axistitlesize     <- 20
# axislabelsize     <- 12
# colTreatment      <- "compound"
# pickThisTreatment <- "All"
# colorbyTreatment  <- TRUE
plot_normalized <- function(df, colX, colY, colsdY, title = NULL, xlab = NULL, ylab = "y", axistitlesize = 20, axislabelsize = 12, colTreatment = "None", pickThisTreatment = "All", colorbyTreatment = FALSE){

    # Subset data frame
    if(!("All" %in% pickThisTreatment)){
        df <- df[df[[colTreatment]] %in% pickThisTreatment,]
    }
    
    limits <- aes(ymax = df[[colY]] + df[[colsdY]], ymin = df[[colY]] - df[[colsdY]])
    
    g <- ggplot(df, aes_string(x = colX, y = colY)) +
        geom_bar(stat="identity") +
        geom_errorbar(limits, width = 0.2, size = 0.5) + 
        geom_hline(yintercept = 100, size = 1, alpha = 0.5, linetype = 2) +
        labs(title = title) +
        labs(x = xlab, y = ylab) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        theme(axis.text=element_text(size = axislabelsize)) + 
        theme(axis.title=element_text(size = axistitlesize, face = "bold"))
    
    # Color bars
    if(colorbyTreatment){
        g <- g + aes_string(fill = colTreatment)
    }
    
    g
}






#############################################
#  Interaction with Fiji
#############################################


# df           <- dget("c:/users/hugo/desktop/dataset.rdata")
# targetWells  <- c(1,2)
# colWell      <- "Metadata_wellNum"
# colsFilePath <- c("OA_path_rawimage", "OA_path_OAmask", "OA_path_OAlabel")
# LUTs         <- c("Green", "glasbey_inverted", "Grays")
# fileSuffixes <- c("C00.tif", "--OAmask", "--OAlabel")
# timeInterval <- 10

# This function generates an ImageJ macro which open the images. The function returns the path to the macro file.
# fileSuffixes can be "[]" for no suffix
FIJI_openOrganoidImages <- function(df, targetWells, colWell, colsFilePath, LUTs, fileSuffixes, timeInterval){
    
    # Check arguments
    unknownCols <- setdiff(colsFilePath, names(df))
    # unknownLuts <- setdiff(LUTs, c("Fire", "Grays", "Ice", "Spectrum", "3-3-2 RGB", "Red", "Green", "Blue", "Cyan", "Magenta", "Yellow", "Red/Green", "16 colors", "5 ramps", "6 shades", "blue orange icb", "brgbcmyw", "cool", "Cyan Hot", "edges", "gem", "glasbey", "glasbey_inverted", "glow", "Green Fire Blue", "HiLo", "ICA", "ICA2", "ICA3", "Magenta Hot", "mpl-inferno", "mpl-magma", "mpl-plasma", "mpl-viridis", "Orange Hot", "phase", "physics", "Rainbow RGB", "Red Hot", "royal", "sepia", "smart", "thal", "thallium", "Thermal", "unionjack", "Yellow Hot"))
    if(length(unknownCols) != 0) stop(paste0("These columns are not part of the dataset: ", paste(unknownCols, collapse = ", ")))
    # if(length(unknownLuts) != 0) stop(paste0("These LUTs are not supported: ", paste(unknownLuts, collapse = ", ")))
    if(length(colsFilePath) != length(fileSuffixes)) stop(paste0("The number of file suffixes (", length(fileSuffixes), ") does not match the number of files (", length(colsFilePath), ")"))
    
    targetWells <- sort(targetWells)
    
    # Subset data frame with file names
    fiji_df <- df[df[[colWell]] %in% targetWells, c(colWell, colsFilePath)]
    fiji_df <- fiji_df[order(fiji_df[[colWell]]),]
    
    
    
    # The following lines write an *ijm macro file containing all instructions and execute it.
    
    # Initialize macro file
    macrofile <- paste0(tempfile(), ".ijm")
    file.create(macrofile)
    writeLines("", macrofile)
    
    # Initialize macro
    macro <- c()
    
    # Go at each well and process the images it contains
    for(well in targetWells){
        
        fiji_df_w <- fiji_df[fiji_df[[colWell]] == well, ]            
        fiji_df_w <- fiji_df_w[order(fiji_df_w[[colsFilePath[1]]]),]

        df_imagepaths <- by(fiji_df_w, fiji_df_w[[colsFilePath[1]]], function(i) i[1, colsFilePath, drop = FALSE], simplify = FALSE)
        df_imagepaths <- do.call("rbind", df_imagepaths)
        
        # Enable batch mode while loading images
        macro <- c(macro, FIJImacro_setBatchMode(TRUE))
        
        # Open each image type in its own separate channel
        for(i in seq_along(colsFilePath)){
            
            # IJ macro: Open raw image stack
            macro <- c(macro, FIJImacro_openFiles(df_imagepaths[, colsFilePath[i]]))
            macro <- c(macro, FIJImacro_imagesToStack(name = colsFilePath[i], title = fileSuffixes[i], useTitlesAsLabels = TRUE, keepSourceImages = FALSE))
            macro <- c(macro, FIJImacro_make8bit())

            # If a labels image exist, insert a time stamp
            if(i == 3){
                macro <- c(macro, FIJImacro_setForegroundRGB(255, 255, 255))
                macro <- c(macro, FIJImacro_timeStamper(timeInterval = timeInterval))
            }
            
        }
        
        # IJ macro: Merge channels
        macro <- c(macro, FIJImacro_mergeChannels(colsFilePath))
        macro <- c(macro, FIJImacro_renameImage("Composite"))

        # IJ macro: make sure the T & Z dimensions are properly interpreted
        macro <- c(macro, FIJImacro_selectWindow("Composite"))
        macro <- c(macro, FIJImacro_setZT(z = 1, t = length(df_imagepaths[, colsFilePath[1]])))
        
        # IJ macro: Rename timelapse
        macro <- c(macro, FIJImacro_selectWindow("Composite"))
        macro <- c(macro, FIJImacro_renameImage(fiji_df_w[1, colsFilePath[1]]))
        
        # IJ macro: Adjust dimensions
        #numtimepoints <- length(unique(fiji_df$analyst_path_rawimage))
        #macro <- c(macro, FIJImacro_adjustZTC(nChannels = 3, nSlices = 1, nTimes = numtimepoints))
        
        # Set LUTs
        for(i in length(colsFilePath):1){
            # IJ macro: Set LUT
            macro <- c(macro, FIJImacro_setLUT(LUT = LUTs[i], slice = i))
        }
        
        # Disable batch mode after loading images
        macro <- c(macro, FIJImacro_setBatchMode(FALSE))
        macro <- c(macro,"")
        
    }
    
    # IJ macro: Display Brightness & Color tools
    macro <- c(macro, FIJImacro_showBrightnessContrast())
    macro <- c(macro, FIJImacro_showChannelsTool())
    
    
    # Write macro to file
    cat(macro, file = macrofile, sep="\n", append = TRUE)
    
    
    # Return path to macro file
    macrofile
}






# These functions write lines of an ImageJ macro

# 'filePath' must be a character vector
FIJImacro_openFiles <- function(filePath){
    
    # Initialize output
    result <- c()

    
    # Replace unwanted characters
    filePath <- gsub("\\\\", "/", filePath)
    filePath <- gsub("%20", " ", filePath)
    filePath <- gsub("^file:///(.*)$", "\\1", filePath)
    
    
    # Populate the results vector
    for (path in filePath){
        
        if(file.exists(path)){
            result <- c(result, 
                        paste0("open(\"", path, "\");"))
        } else{
            
            # Missing files are rendered as black images
            result <- c(result,
                        paste0("newImage(\"", basename(path), "\", \"8-bit black\", 10, 10, 1);"))
            #paste0("print(\"File not found:\" + ", path, ");"))
        }
        
    }
    
    # Return result
    result
}

FIJImacro_make8bit <- function(){
    "run(\"8-bit\");"
}

FIJImacro_imagesToStack <- function(name = "Stack", title = "[]", useTitlesAsLabels = TRUE, keepSourceImages = TRUE){
    
    result <- paste0("run(\"Images to Stack\", \"method=[Copy (center)] name=",
                     name,
                     " title=",
                     title)
    
    if(useTitlesAsLabels){
        result <- paste0(result, " use")
    }
    
    if(keepSourceImages){
        result <- paste0(result, " keep")
    } 
    
    result <- paste0(result, "\");")
    
    result
}

FIJImacro_setForegroundRGB <- function(r, g, b){
    
    result <- paste0("setForegroundColor(", r, ", ", g, ", ", b, ");")
    
    result
}

FIJImacro_timeStamper <- function(timeStart = 0, timeInterval, x = 2, y = 15, fontSize = 12, decimalFigures = 0, unit = "min"){
    
    result <- paste0("run(\"Time Stamper\", \"starting=", timeStart, " interval=", timeInterval, " x=", x, " y=", y, " font=", fontSize, " decimal=", decimalFigures, " anti-aliased or=", unit, "\");")
    
    result
}

# 'img_names' must be a character vector
FIJImacro_mergeChannels <- function(img_names, createComposite = TRUE, keepSource = FALSE, ignoreLUT = FALSE){
    
    result <- "run(\"Merge Channels...\", \""
    
    for(i in 1:length(img_names)){
        result <- paste0(result, "c", i, "=", img_names[i], " ")
    }
    
    # trim trailing space
    result <- gsub("^(.*) $", "\\1", result)
    
    if(createComposite){
        result <- paste0(result, " create")
    }
    
    if(keepSource){
        result <- paste0(result, " keep")
    }
    
    if(ignoreLUT){
        result <- paste0(result, " ignore")
    }
    
    result <- paste0(result, "\");")
    
    result
}

FIJImacro_selectWindow <- function(windowname){
    
    result <- paste0("selectWindow(\"", windowname, "\");")
    
    result
}

# Sets the number of z slices and time points in an image
FIJImacro_setZT <- function(z, t){
    result <- paste0("run(\"Properties...\", \"slices=", z, " frames=", t, "\");")
    
    result
}

FIJImacro_renameImage <- function(name = "new image"){
    
    result <- paste0("rename(\"", name, "\");")
    
    result
}

FIJImacro_adjustZTC <- function(nChannels = 1, nSlices = 1, nTimes = 1){
    
    
    result <- "run(\"Properties...\", \""
    
    if(!missing(nChannels)){
        result <- paste0(result, "channels=", nChannels, " ")
    }
    
    if(!missing(nSlices)){
        result <- paste0(result, "slices=", nSlices, " ")
    }
    
    if(!missing(nTimes)){
        result <- paste0(result, "frames=", nTimes, " ")
    }
    
    # Trim trailing space
    result <- gsub("^(.*) ", "\\1", result)
    
    result <- paste0(result, "\");")
    
    result
}

# 'LUT' must be a valid LUT name in ImageJ
FIJImacro_setLUT <- function(LUT = "Grays", slice = 1){
    
    result <- paste0("setSlice(", slice, ");")
    result <- c(result, 
                paste0("run(\"", LUT, "\");"))
    
    result
}

FIJImacro_setBatchMode <- function(boolean){
    
    if(boolean){
        parameter <- "true"
    } else{
        parameter <- "false"
    }
    result <- paste0("setBatchMode(", parameter, ");")
    
    result
}

FIJImacro_showBrightnessContrast <- function(){
    
    result <- "run(\"Brightness/Contrast...\");"
    
    result
}

FIJImacro_showChannelsTool <- function(){
    
    result <- "run(\"Channels Tool...\");"
    
    result
}



#############################################
#  Analysis functions
#############################################

# df           <- dataset
# colWell      <- "Metadata_wellNum"
# colTimePoint <- "Metadata_timeNum"
# colArea      <- "Math_area_micronsq"
# colCompound  <- "Metadata_compound"
# colConc      <- "Metadata_concentration"
# colID        <- "TrackObjects_Label_2"
# nameInvalid  <- "NaN"
# numRows      <- 8
# numCols      <- 12
# timeRes      <- 10
#
# Calculates for each well and timepoint:
#    * Total area
#    * Normalized area (Area @ 0min == 100%)
#    * Normalized area with offset (Area @ 0min == 0%)
#    * cumultive AUC
normalizeFIS <- function(df, colWell, colTimePoint, colArea, colCompound, colConc, colID, nameInvalid, numRows, numCols, timeRes){
    
    
    # Initialize variables
    colTrueTime <- "analyst_min"
    
    
    # Discard unnecessary columns
    df <- df[,c(colWell, colTimePoint, colCompound, colConc, colArea, colID)]
    
    
    # Embed 'true' time values onto the data.frame
    message(paste0("Generating 'true' time values: ", timeRes, " minutes per datapoint"))
    df[[colTrueTime]] <- (df[[colTimePoint]] - min(df[[colTimePoint]])) * timeRes
    message("    Finished generating true time values")
    message("")

    
    
    ### Get metadata and total area ###
    
    message("    Initializing results table")
    AllWells      <- 1:(numRows * numCols)
    AllTimePoints <- unique(df[[colTrueTime]])
    AllWellTime   <- expand.grid(AllTimePoints, AllWells)
    
    # Go through all well+time combinations and get data
    results0 <- apply(AllWellTime, 1, function(x){

        # Subset data pertaining to all well+time combinations
        w <- x["Var2"]
        t <- x["Var1"]
        relevantdata <- df[df[[colWell]] == w & df[[colTrueTime]] == t,]
        
        
        # Remove invalid organoids. These are typically organoids which were not tracked throughout the whole experiment
        nOrganoids0       <- nrow(relevantdata)
        relevantdata      <- relevantdata[relevantdata[[colID]] != nameInvalid, ]
        nOrganoids1       <- nrow(relevantdata)
        nOrganoidsInvalid <- nOrganoids0 - nOrganoids1
        message(paste0("        Well ", w, " [time ", t, "]: ", nOrganoidsInvalid, " organoids discarded."))
        
        compound      <- ifelse(length(unique(relevantdata[[colCompound]])) == 0, NA, unique(relevantdata[[colCompound]]))
        concentration <- ifelse(length(unique(relevantdata[[colConc]])) == 0, NA, unique(relevantdata[[colConc]]))
        if(length(compound) > 1) stop(paste0("Multiple compounds detected in well ", w, ": ", paste(compound, collapse = ";")))
        if(length(concentration) > 1) stop(paste0("Multiple concentrations detected in well ", w, ": ", paste(compound, collapse = ";")))
        treatment     <- ifelse(is.na(compound) & is.na(concentration), NA, paste0(compound, "_", concentration))
        sumarea       <- ifelse(nrow(relevantdata) == 0, NA, sum(relevantdata[[colArea]], na.rm = TRUE))
        
        # Produce results data frame
        data.frame(well             = w,
                   time             = t,
                   compound         = compound,
                   concentration    = concentration,
                   treatment        = treatment,
                   sumarea          = sumarea,
                   stringsAsFactors = FALSE)
    })
    
    results0 <- do.call("rbind", results0)
    message("    Finished adding treatment and total area")
    message("")
    
    
    
    ### Compute normalized area ###
    message("    Normalizing data")
    results <- by(results0, results0$well, function(x){
        
        w <- x[1,"well"]
        message(paste0("        Analyzing well ", w))

        x$normalized        <- normalize100(x$time, x$sumarea)
        x$normalized_offset <- x$normalized - 100
        x$cumulative_AUC    <- cumulativeAUC(x$time, x$normalized_offset)
        x
    })
    results <- do.call("rbind", results)
    
    message("    Finished normalizing data")
    results
}


# df       <- resultsFIS
# colWell  <- "well"
# colTime  <- "time"
# colData  <- "normalized"
# t0       <- 0
# tf       <- 24
# colSlope <- "initialSlope"
# Adds initial slopes by fitting a linear model between t0 and tf
addslopesFIS <- function(df, colWell, colTime, colData, t0, tf, colSlope = "initialSlope"){
    
    df[[colSlope]] <- NA
    
    AllWells <- unique(df[[colWell]])
    
    for (w in AllWells){
      
        # get data for a single well. Must match the [t0,tf] interval
        relevantdata <- df[df[[colWell]] == w & !is.na(df[[colData]]) & df[[colTime]] >= t0 & df[[colTime]] <= tf, c(colWell, colTime, colData)]
        
        # Populate results data frame
        dataX <- relevantdata[[colData]]
        dataY <- relevantdata[[colTime]]
        
        df[df[[colWell]] == w, colSlope] <- if(length(dataX) * length(dataY) == 0){
            NA
        } else{
            lm(dataX ~ dataY)$coefficients[2]
        }
        
    }
    
    df
}





#############################################
#  Helper functions
#############################################


# Normalizes a time series such that the initial value becomes 100
normalize100 <- function(time, values){
    
    # Both parameters must be numeric vectors with the same length
    
    # ====INPUT=====
    # Time  area
    # 0		8029.734
    # 8		8301.062
    # 16    8548.796
    # 24    8902.702
    
    # ====OUTPUT====
    # 100.0000
    # 103.3790
    # 106.4643
    # 110.8717
    
    
    # Check if both arguments are numeric and have the same length
    if (!is.numeric(time)) {stop("Argument 'time' is not numeric.")}
    if (!is.numeric(values)) {stop("Argument 'values' is not numeric.")}
    if (length(time) != length(values)) {stop("Arguments have different lengths.")}
    
    # index of the initial (i.e. lowest) time point
    i0 <- which(time == min(time))
    normalized <- values/values[i0] * 100
    names(normalized) <- time
    normalized
}



# Calculates the AUC for increasingly larger curves: [x0,y0 : x0,y0], [x0,y0 : x1,y1], [x0,y0 : x2,y2], [x0,y0 : x3,y3], ...
cumulativeAUC <- function(x, y){

    # ====EXAMPLE1=====
    # time	area    output
    # 0     0       0
    # 8		3       12
    # 16	6       48
    # 24	10      112
    
    # ====EXAMPLE2 (order does not matter=====
    # time	area    output
    # 8     3       12
    # 16	6       48
    # 0     0       0
    # 24	10      112
    
    # Make sure x and y are numeric, have the same size and contain some data
    # The AUC for a single data point is 0
    if (!is.numeric(x)) {stop("Vector 'x' is not numeric.")}
    if (!is.numeric(y)) {stop("Vector 'y' is not numeric.")}
    if (length(x) != length(y)) {stop ("Vectors have different lengths.")}
    if (length(x) == 0) {stop ("Empty vectors.")}
    if (length(x) == 1) {return(0)}

    # Make sure x & y are sorted
    u <- order(x)
    x1 <- x[u]
    y1 <- y[u]
    
    
    cumulateInt <- numeric(length(x))
    names(cumulateInt) <- x1
    if(is.na(y1[1])){
        cumulateInt[1] <- NA
    } else{
        cumulateInt[1] <- 0 
    }
       
    
    for (i in 2:length(x)){
        # Integration range
        IntRange <- 1:i
        # Function trapz belongs to package "caTools"
        cumulateInt[i] <- trapz(x1[IntRange], y1[IntRange])
    }
    
    cumulateInt <- cumulateInt[order(u)]    # return AUC values in the same order as 'x'
    cumulateInt
}



# Calculates the slope of fitting a linear model to increasingly larger curves: [x0,y0 : x0,y0], [x0,y0 : x1,y1], [x0,y0 : x2,y2], [x0,y0 : x3,y3], ...
initial.slopes <- function(x,y){
    
    # Both parameters must be numeric vectors with the same length
    
    # ====INPUT=====
    # x     y
    # 0.0	0.1
    # 1.0	1.8
    # 2.0	3.8
    # 3.7	5.6
    # 7.0	6.5
    # 13    7.5
    
    # ====OUTPUT====
    # NA
    # 1.7000000
    # 1.8500000
    # 1.4988283
    # 0.8904219
    # 0.5135525
    
    # Check if both arguments are numeric and have the same length
    if (length(x) != length(y)) stop("Arguments have different lengths.")
    if (length(x) == 0) return(NA)
    if (!is.numeric(x)) stop("Argument 'x' is not numeric.")
    if (!is.numeric(y)) {
        slopes <- rep(NA, length(y))
        names(slopes) <- x
        return(slopes)
    }
    
    
    slopes <- rep(NA, length(x))
    names(slopes) <- x

    
    for (i in 2:length(x)){
        
        # Set range of data points for the linear model
        okValues <- y[1:i]
        names(okValues) <- x[1:i]
        okValues <- okValues[!is.na(okValues)]

        # Calculate the slope of the linear model including the firs i data points
        slopes[i] <- if(length(okValues) == 0){
            NA
        } else{
            lm(okValues ~ as.numeric(names(okValues)))$coefficients[2]
        }

    }
    
    slopes
}



# Generates a 'convenience matrix' expressing the value of a variable as a function of well and time
reshapeFIS <- function(df, colWell, colTime, colVariable, colCompound, colConcentration, colTreatment){

    # Extract data
    wideby <- by(df, df[[colTime]], function(x){
        temp <- x[[colVariable]]
        names(temp) <- x[[colWell]]
        temp <- temp[order(as.numeric(names(temp)))]
    })
    
    datamat  <- as.matrix(do.call("rbind", wideby))
    roworder <- order(as.numeric(row.names(datamat)))
    colorder <- order(as.numeric(colnames(datamat)))
    datamat  <- datamat[roworder, colorder, drop=F]
    class(datamat) <- "character"
    
    # add labels
    well          <- colnames(datamat)
    compound      <- sapply(well, function(x) df[df[[colWell]] == x, colCompound][1])
    concentration <- sapply(well, function(x) df[df[[colWell]] == x, colConcentration][1])
    treatment     <- sapply(well, function(x) df[df[[colWell]] == x, colTreatment][1])

    # Export
    rbind(well,
          compound,
          concentration,
          treatment,
          datamat)
    
}



# Generates a blank image in the 'magick' format.
blank_magick <- function(width = 512, height = 512){
    
    # Requires the 'magick' and 'raster' packages
    
    # Initialize a magick image device
    fig <- image_graph(width = width, height = height)
    
    # Create image
    m = matrix(0,10,10)
    par(mar = rep(0, 4))
    image(m, axes = FALSE)
    
    # Close image device
    dev.off()
    
    # Return a black image
    fig <- image_fill(fig, "black", fuzz = 256^3)
    fig
}



# Returns the width of a number when printed in an image (in pixels)
pixelsWide <- function(number){
    
    chrlength <- data.frame(num = 0:9,
                            len = c(5,3,5,5,5,5,5,5,5,5))
    chrgap <- 1
    
    # Process a non-integer input
    if(is.na(number)) return(10)
    
    if(!is.integer(number)){
        x <- as.character(number)
        return(nchar(x)*5 + chrgap*(nchar(x)-1))
    }

    
    res <- 0
    
    for(i in 1:nchar(number)){
        onechr <- as.integer(substr(number,i,i))
        res <- res + chrlength[chrlength$num == onechr, "len"]
    }
    
    res + chrgap * (nchar(number) - 1)
}



# Returns the height of a number when printed in an image (in pixels)
pixelsTall <- function(number){
    7
}