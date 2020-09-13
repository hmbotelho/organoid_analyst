OAversion <- "1.0.0"

shinyServer(function(input, output, session) {

    # 0. Initialize variables ------------------------------------

    OA <- list(
        dataset.raw                   = do.call(cbind,rep(data.frame(numeric()),9)),
        dataset.norm                  = do.call(cbind,rep(data.frame(numeric()),10)),
        dataset.norm.QC               = do.call(cbind,rep(data.frame(numeric()),10)),
        summary.slope                 = do.call(cbind,rep(data.frame(numeric()),7)),
        summary.auc                   = do.call(cbind,rep(data.frame(numeric()),7)),
        summary.areachange            = do.call(cbind,rep(data.frame(numeric()),7)),
        sourcefolder                  = "C:/sample_dataset/demoplate_01",
        outputfolder                  = "C:/sample_dataset/demoplate_01--cellprofiler--analysis",
        Settings_objFileName          = "objects.csv",
        Settings_TimeRes              = 10,
        Settings_colArea              = "Math_area_micronsq",
        Settings_colTime              = "Metadata_timeNum",
        Settings_colWell              = "Metadata_wellNum",
        Settings_colCompound          = "Metadata_compound",
        Settings_colConcentration     = "Metadata_concentration",
        Settings_numRows              = 8,
        Settings_numCols              = 12,
        Settings_colOrganoidID        = "TrackObjects_Label_4",
        Settings_nameInvalid          = "Allow all organoids",
        Settings_colX                 = "AreaShape_Center_X",
        Settings_colY                 = "AreaShape_Center_Y",
        Settings_colRawImgPath        = "Metadata_FileLocation",
        Settings_pathInTable          = "file:///C:/FIS",
        Settings_pathInComputer       = "C:\\FIS",
        Settings_generateMasks        = FALSE,
        Settings_MimagepathInTable    = "file:///C:/FIS/demo_dataset/03-images_renamed/demoplate_01",
        Settings_MimagepathInComputer = "C:\\FIS\\demo_dataset\\05-images_analysis\\demoplate_01--cellprofiler",
        Settings_rawSuffixLength      = 9,
        Settings_maskFileSuffix       = "--masks.png",
        Settings_OAMasksFileSuffix    = "--OAmask",
        Settings_OALabelsFileSuffix   = "--OAlabel",
        fiji_binary                   = "C:/Fiji.app/ImageJ-win64.exe",
        slider_initial                = c(10,30),
        slider_tf                     = 60,
        p1_height                     = 600,
        p1_showFit                    = TRUE,
        p1_showLUT                    = TRUE,
        p1_highlight                  = "None",
        p1_labelSize                  = 3,
        p2_selectTreatment            = "All",
        p3_selectTreatment            = "All",
        p3_showLUT                    = FALSE,
        p4_selectTreatment            = "All",
        p4_showLUT                    = FALSE,
        p5_selectTreatment            = "All",
        p5_showLUT                    = FALSE,
        QClist                        = NULL,
        targetWells                   = NULL,
        colInitSlope                  = "initialSlope"
    )
    colnames(OA[["dataset.raw"]])        <- c(OA[["Settings_colArea"]], OA[["Settings_colTime"]], OA[["Settings_colWell"]],
                                              OA[["Settings_colCompound"]], OA[["Settings_colConcentration"]], OA[["Settings_colOrganoidID"]],
                                              OA[["Settings_colX"]], OA[["Settings_colY"]], OA[["Settings_colRawImgPath"]])
    colnames(OA[["dataset.norm"]])       <- c("well", "time", "compound", "concentration", "treatment", "sumarea",
                                              "normalized", "normalized_offset", "cumulative_AUC", OA[["colInitSlope"]])
    colnames(OA[["dataset.norm.QC"]])    <- colnames(OA[["dataset.norm"]])
    colnames(OA[["summary.slope"]])      <- c("treatment", "compound", "concentration", "mean", "sd", "n", "SEM")
    colnames(OA[["summary.auc"]])        <- c("treatment", "compound", "concentration", "mean", "sd", "n", "SEM")
    colnames(OA[["summary.areachange"]]) <- c("treatment", "compound", "concentration", "mean", "sd", "n", "SEM")


    # Keep the OA object updated
    observeEvent(input$Settings_objFileName,          {OA[["Settings_objFileName"]]          <<- input$Settings_objFileName})
    observeEvent(input$Settings_TimeRes,              {OA[["Settings_TimeRes"]]              <<- input$Settings_TimeRes})
    observeEvent(input$Settings_colArea,              {OA[["Settings_colArea"]]              <<- input$Settings_colArea})
    observeEvent(input$Settings_colTime,              {OA[["Settings_colTime"]]              <<- input$Settings_colTime})
    observeEvent(input$Settings_colWell,              {OA[["Settings_colWell"]]              <<- input$Settings_colWell})
    observeEvent(input$Settings_colCompound,          {OA[["Settings_colCompound"]]          <<- input$Settings_colCompound})
    observeEvent(input$Settings_colConcentration,     {OA[["Settings_colConcentration"]]     <<- input$Settings_colConcentration})
    observeEvent(input$Settings_numRows,              {OA[["Settings_numRows"]]              <<- input$Settings_numRows})
    observeEvent(input$Settings_numCols,              {OA[["Settings_numCols"]]              <<- input$Settings_numCols})
    observeEvent(input$Settings_colOrganoidID,        {OA[["Settings_colOrganoidID"]]        <<- input$Settings_colOrganoidID})
    observeEvent(input$Settings_nameInvalid,          {OA[["Settings_nameInvalid"]]          <<- input$Settings_nameInvalid})
    observeEvent(input$Settings_colX,                 {OA[["Settings_colX"]]                 <<- input$Settings_colX})
    observeEvent(input$Settings_colY,                 {OA[["Settings_colY"]]                 <<- input$Settings_colY})
    observeEvent(input$Settings_colRawImgPath,        {OA[["Settings_colRawImgPath"]]        <<- input$Settings_colRawImgPath})
    observeEvent(input$Settings_pathInTable,          {OA[["Settings_pathInTable"]]          <<- input$Settings_pathInTable})
    observeEvent(input$Settings_pathInComputer,       {OA[["Settings_pathInComputer"]]       <<- input$Settings_pathInComputer})
    observeEvent(input$Settings_generateMasks,        {OA[["Settings_generateMasks"]]        <<- input$Settings_generateMasks})
    observeEvent(input$Settings_MimagepathInTable,    {OA[["Settings_MimagepathInTable"]]    <<- input$Settings_MimagepathInTable})
    observeEvent(input$Settings_MimagepathInComputer, {OA[["Settings_MimagepathInComputer"]] <<- input$Settings_MimagepathInComputer})
    observeEvent(input$Settings_rawSuffixLength,      {OA[["Settings_rawSuffixLength"]]      <<- input$Settings_rawSuffixLength})
    observeEvent(input$Settings_maskFileSuffix,       {OA[["Settings_maskFileSuffix"]]       <<- input$Settings_maskFileSuffix})
    observeEvent(input$Settings_OAMasksFileSuffix,    {OA[["Settings_OAMasksFileSuffix"]]    <<- input$Settings_OAMasksFileSuffix})
    observeEvent(input$Settings_OALabelsFileSuffix,   {OA[["Settings_OALabelsFileSuffix"]]   <<- input$Settings_OALabelsFileSuffix})
    observeEvent(input$fiji_binary,                   {OA[["fiji_binary"]]                   <<- input$fiji_binary})
    observeEvent(input$slider_initial,                {OA[["slider_initial"]]                <<- input$slider_initial})
    observeEvent(input$slider_tf,                     {OA[["slider_tf"]]                     <<- input$slider_tf})
    observeEvent(input$p1_height,                     {OA[["p1_height"]]                     <<- input$p1_height})
    observeEvent(input$p1_showFit,                    {OA[["p1_showFit"]]                    <<- input$p1_showFit})
    observeEvent(input$p1_showLUT,                    {OA[["p1_showLUT"]]                    <<- input$p1_showLUT})
    observeEvent(input$p1_highlight,                  {OA[["p1_highlight"]]                  <<- input$p1_highlight})
    observeEvent(input$p1_labelSize,                  {OA[["p1_labelSize"]]                  <<- input$p1_labelSize})
    observeEvent(input$p2_selectTreatment,            {OA[["p2_selectTreatment"]]            <<- input$p2_selectTreatment})
    observeEvent(input$p3_selectTreatment,            {OA[["p3_selectTreatment"]]            <<- input$p3_selectTreatment})
    observeEvent(input$p3_showLUT,                    {OA[["p3_showLUT"]]                    <<- input$p3_showLUT})
    observeEvent(input$p4_selectTreatment,            {OA[["p4_selectTreatment"]]            <<- input$p4_selectTreatment})
    observeEvent(input$p4_showLUT,                    {OA[["p4_showLUT"]]                    <<- input$p4_showLUT})
    observeEvent(input$p5_selectTreatment,            {OA[["p5_selectTreatment"]]            <<- input$p5_selectTreatment})
    observeEvent(input$p5_showLUT,                    {OA[["p5_showLUT"]]                    <<- input$p5_showLUT})
    observeEvent(input$QClist,                        {OA[["QClist"]]                        <<- input$QClist})
    observeEvent(input$targetWells,                   {OA[["targetWells"]]                   <<- input$targetWells})


    # Initialize folder handling
    volumes <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "Settings_folderInput", roots = volumes, session = session, restrictions = system.file(package = "base"))
    shinyDirChoose(input, "Settings_folderOutput", roots = volumes, session = session, restrictions = system.file(package = "base"))
    




    # 1. Data loading ------------------------------------

    ### Import a CellProfiler data folder

    observeEvent(input$Settings_folderInput, {

        # Make sure to respond only if the user has selcted a folder
        if(!is.integer(input$Settings_folderInput)){


            # Find out which folder the user has selected
            temp <- parseDirPath(volumes, input$Settings_folderInput)
            OA[["sourcefolder"]] <<- sub("/*$", "", temp)
            OA[["outputfolder"]] <<- paste0(OA[["sourcefolder"]], "--analysis")

            # Concatenate object files
            {withCallingHandlers({
                html("echo_concat", "", add = FALSE)
                message(paste0("Concatenating object files in '", OA[["sourcefolder"]], "':"))
                OA$dataset.raw <<- concatenate.csv(folder      = OA[["sourcefolder"]],
                                                   pattern     = gsub("\\.", "\\\\.", OA[["Settings_objFileName"]]),
                                                   recursive   = TRUE,
                                                   parallelize = TRUE)
                message("Finished concatenating")
                message("")

                output$table_rawdata <- DT::renderDataTable(OA[["dataset.raw"]])
                message("The dataset can be viewed in 'View Data > Raw data table'")

                # Navigate to next tab
                updateNavbarPage(session, "mainNavbarPage", selected = "tabSettings")
            },
                message = function(m) html("echo_concat", m$message, add = TRUE)
            )}

            # Initialize variables: Column names
            if(!(OA[["Settings_colArea"]] %in% names(OA[["dataset.raw"]])))          OA[["Settings_colArea"]]          <<- names(OA[["dataset.raw"]])[1]
            if(!(OA[["Settings_colTime"]] %in% names(OA[["dataset.raw"]])))          OA[["Settings_colTime"]]          <<- names(OA[["dataset.raw"]])[1]
            if(!(OA[["Settings_colWell"]] %in% names(OA[["dataset.raw"]])))          OA[["Settings_colWell"]]          <<- names(OA[["dataset.raw"]])[1]
            if(!(OA[["Settings_colCompound"]] %in% names(OA[["dataset.raw"]])))      OA[["Settings_colCompound"]]      <<- names(OA[["dataset.raw"]])[1]
            if(!(OA[["Settings_colConcentration"]] %in% names(OA[["dataset.raw"]]))) OA[["Settings_colConcentration"]] <<- names(OA[["dataset.raw"]])[1]
            if(!(OA[["Settings_colOrganoidID"]] %in% names(OA[["dataset.raw"]])))    OA[["Settings_colOrganoidID"]]    <<- names(OA[["dataset.raw"]])[1]
            if(!(OA[["Settings_colX"]] %in% names(OA[["dataset.raw"]])))             OA[["Settings_colX"]]             <<- names(OA[["dataset.raw"]])[1]
            if(!(OA[["Settings_colY"]] %in% names(OA[["dataset.raw"]])))             OA[["Settings_colY"]]             <<- names(OA[["dataset.raw"]])[1]
            if(!(OA[["Settings_colRawImgPath"]] %in% names(OA[["dataset.raw"]])))    OA[["Settings_colRawImgPath"]]    <<- names(OA[["dataset.raw"]])[1]
            # Initialize variables: Fiji related
            if(Sys.info()['sysname'] == "Darwin")  OA[["fiji_binary"]] <<- "/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx"
            if(Sys.info()['sysname'] == "Windows") OA[["fiji_binary"]] <<- OA[["fiji_binary"]]
            # Initialize variables: Raw images
            sample_rawImagePath      <<- reactive({
                OA[["dataset.raw"]][1,OA[["Settings_colRawImgPath"]]]
            })
            sample_remappedImagePath <<- reactive({
                pathInTable         <- ifelse(is.null(input$Settings_pathInTable),
                                              OA[["Settings_pathInTable"]],
                                              input$Settings_pathInTable)
                pathInTable         <- sub("^file:///", "", pathInTable)
                pathInComputer      <- ifelse(is.null(input$Settings_pathInComputer),
                                              OA[["Settings_pathInComputer"]],
                                              input$Settings_pathInComputer)

                sample_rawImagePath <- sample_rawImagePath()
                sample_rawImagePath <- sub("^file:///", "", sample_rawImagePath)
                sample_rawImagePath <- gsub("\\\\", "/", sample_rawImagePath)

                pathInTable         <- gsub("\\\\", "/", pathInTable)
                pathInComputer      <- gsub("\\\\", "/", pathInComputer)
                output              <- sub(pathInTable, pathInComputer, sample_rawImagePath)
                output
            })
            exists_remappedImagePath <<- reactive({
                file.exists(sample_remappedImagePath())
            })
            # Initialize variables: Mask/label images
            sample_rawImageSuffix    <<- reactive({
                if(is.null(input$Settings_rawSuffixLength)) return(OA[["Settings_rawSuffixLength"]])

                sample_rawImagePath <- sample_rawImagePath()

                output <- substring(sample_rawImagePath,
                                    nchar(sample_rawImagePath) - input$Settings_rawSuffixLength + 1,
                                    nchar(sample_rawImagePath))
                output
            })
            sample_remappedMaskPath  <<- reactive({
                rawImagePath   <- sample_rawImagePath()
                pathInTable    <- ifelse(is.null(input$Settings_MimagepathInTable),
                                         OA[["Settings_MimagepathInTable"]],
                                         input$Settings_MimagepathInTable)
                pathInTable    <- sub("^file:///", "", pathInTable)
                pathInComputer <- ifelse(is.null(input$Settings_MimagepathInComputer),
                                         OA[["Settings_MimagepathInComputer"]],
                                         input$Settings_MimagepathInComputer)
                rawSuffix      <- sample_rawImageSuffix()
                maskSuffix     <- ifelse(is.null(input$Settings_maskFileSuffix),
                                         OA[["Settings_maskFileSuffix"]],
                                         input$Settings_maskFileSuffix)

                rawImagePath   <- sub("^file:///", "", rawImagePath)
                rawImagePath   <- gsub("\\\\", "/", rawImagePath)
                pathInTable    <- gsub("\\\\", "/", pathInTable)
                pathInComputer <- gsub("\\\\", "/", pathInComputer)

                output         <- sub(pathInTable, pathInComputer, rawImagePath)
                output         <- sub(paste0(rawSuffix, "$"),
                                      maskSuffix,
                                      output)
                output
            })
            exists_remappedMaskPath  <<- reactive({
                file.exists(sample_remappedMaskPath())
            })
            sample_newMaskPath       <<- reactive({
                maskPath  <- sample_remappedMaskPath()
                oldSuffix <- ifelse(is.null(input$Settings_maskFileSuffix),
                                    OA[["Settings_maskFileSuffix"]],
                                    input$Settings_maskFileSuffix)
                newSuffix <- ifelse(is.null(input$Settings_OAMasksFileSuffix),
                                    OA[["Settings_OAMasksFileSuffix"]],
                                    input$Settings_OAMasksFileSuffix)

                output <- sub(oldSuffix,
                              paste0(newSuffix, ".png"),
                              maskPath)
                output
            })
            sample_newLabelsPath     <<- reactive({
                maskPath  <- sample_remappedMaskPath()
                oldSuffix <- ifelse(is.null(input$Settings_maskFileSuffix),
                                OA[["Settings_maskFileSuffix"]],
                                input$Settings_maskFileSuffix)
                newSuffix <- ifelse(is.null(input$Settings_OALabelsFileSuffix),
                                OA[["Settings_OALabelsFileSuffix"]],
                                input$Settings_OALabelsFileSuffix)

                output <- sub(oldSuffix,
                              paste0(newSuffix, ".png"),
                              maskPath)
                output
            })


            # Render widgets
            output$UI_sourcefolder    <- renderUI({
                tagList(
                    fluidRow(
                        column(11,
                            br(),
                            em(strong("Source folder: "), OA[["sourcefolder"]]),
                            br(),br()
                        )
                    )
                )
            })
            output$UI_outputFolder    <- renderUI({
                tagList(
                    h3("Output Folder"),
                    fluidRow(
                        column(11,
                            shinyDirButton("Settings_folderOutput", "Change output folder...", "Select the folder to keep analysis results", icon = icon("folder")),
                            br(),
                            br(),
                            em(strong("Output folder: "), OA[["outputfolder"]])
                        ),
                        column(1,
                            actionButton("help_Settings_folderOutput", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_folderOutput {margin-top: 0px;}")
                    )
                )
            })
            output$UI_ExpSettings     <- renderUI({
                tagList(
                    h3("Experiment Settings"),
                    fluidRow(
                        column(11,
                            numericInput("Settings_TimeRes", "Time resolution (minutes per timepoint)", value = OA[["Settings_TimeRes"]], width = "100%"),
                            selectInput("Settings_colArea", "Name of the column with AREA values", choices = as.list(names(OA[["dataset.raw"]])), selected = OA[["Settings_colArea"]], width = "100%"),
                            selectInput("Settings_colTime", "Name of the column with TIME values", choices = as.list(names(OA[["dataset.raw"]])), selected = OA[["Settings_colTime"]], width = "100%"),
                            selectInput("Settings_colWell", "Name of the column with WELL values", choices = as.list(names(OA[["dataset.raw"]])), selected = OA[["Settings_colWell"]], width = "100%"),
                            selectInput("Settings_colCompound", "Name of the column with COMPOUND names", choices = as.list(names(OA[["dataset.raw"]])), selected = OA[["Settings_colCompound"]], width = "100%"),
                            selectInput("Settings_colConcentration", "Name of the column with CONCENTRATION values", choices = as.list(names(OA[["dataset.raw"]])), selected = OA[["Settings_colConcentration"]], width = "100%"),
                            numericInput("Settings_numRows", "Number of rows", value = OA[["Settings_numRows"]], width = "100%"),
                            numericInput("Settings_numCols", "Number of columns", value = OA[["Settings_numCols"]], width = "100%")
                        ),
                        column(1,
                            actionButton("help_Settings_TimeRes", "", icon = icon("question-circle")),
                            actionButton("help_Settings_colArea", "", icon = icon("question-circle")),
                            actionButton("help_Settings_colTime", "", icon = icon("question-circle")),
                            actionButton("help_Settings_colWell", "", icon = icon("question-circle")),
                            actionButton("help_Settings_colCompound", "", icon = icon("question-circle")),
                            actionButton("help_Settings_colConcentration", "", icon = icon("question-circle")),
                            actionButton("help_Settings_numRows", "", icon = icon("question-circle")),
                            actionButton("help_Settings_numCols", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_TimeRes {margin-top: 25px;}"),
                        tags$style(type='text/css', "#help_Settings_colArea {margin-top: 40px;}"),
                        tags$style(type='text/css', "#help_Settings_colTime {margin-top: 45px;}"),
                        tags$style(type='text/css', "#help_Settings_colWell {margin-top: 45px;}"),
                        tags$style(type='text/css', "#help_Settings_colCompound {margin-top: 45px;}"),
                        tags$style(type='text/css', "#help_Settings_colConcentration {margin-top: 45px;}"),
                        tags$style(type='text/css', "#help_Settings_numRows {margin-top: 45px;}"),
                        tags$style(type='text/css', "#help_Settings_numCols {margin-top: 40px;}")
                    )
                )
            })
            output$UI_QCSettings1     <- renderUI({
                tagList(
                    h3("Quality Control Settings"),
                    fluidRow(
                        column(11,
                            selectInput("Settings_colOrganoidID", "Name of the column with organoid ID", choices = as.list(names(OA[["dataset.raw"]])), selected = OA[["Settings_colOrganoidID"]], width = "100%")
                        ),
                        column(1,
                            actionButton("help_Settings_colOrganoidID", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_colOrganoidID {margin-top: 25px;}")
                    )
                )
            })
            output$UI_QCSettings2     <- renderUI({
                input$Settings_colOrganoidID
                alllabels <- sort(unique(
                    OA[["dataset.raw"]][,OA[["Settings_colOrganoidID"]]]
                ))
                alllabels <- c("Allow all organoids", alllabels)

                fluidRow(
                    column(11,
                        selectInput("Settings_nameInvalid", "ID of invalid organoids", choices = as.list(alllabels), selected = OA[["Settings_nameInvalid"]], multiple = TRUE, width = "100%"),
                        selectInput("Settings_colX", "Name of the column with organoid center (X)", choices = as.list(names(OA[["dataset.raw"]])), selected = OA[["Settings_colX"]], width = "100%"),
                        selectInput("Settings_colY", "Name of the column with organoid center (Y)", choices = as.list(names(OA[["dataset.raw"]])), selected = OA[["Settings_colY"]], width = "100%")
                    ),
                    column(1,
                        actionButton("help_Settings_nameInvalid", "", icon = icon("question-circle")),
                        actionButton("help_Settings_colX", "", icon = icon("question-circle")),
                        actionButton("help_Settings_colY", "", icon = icon("question-circle"))
                    ),
                    tags$style(type='text/css', "#help_Settings_nameInvalid {margin-top: 25px;}"),
                    tags$style(type='text/css', "#help_Settings_colX {margin-top: 45px;}"),
                    tags$style(type='text/css', "#help_Settings_colY {margin-top: 45px;}")
                )
            })
            output$UI_RemapSettings1  <- renderUI({
                tagList(
                    h3("File Remapping Settings"),
                    tags$i("Find the location of raw microscopy images in this computer to open them in Fiji."),
                    p(" "),
                    fluidRow(
                        column(11,
                            selectInput("Settings_colRawImgPath", "Column with file path", choices = as.list(names(OA[["dataset.raw"]])), selected = OA[["Settings_colRawImgPath"]], width = "100%")
                        ),
                        column(1,
                            actionButton("help_Settings_colRawImgPath", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_colRawImgPath {margin-top: 25px;}")
                    )
                )
            })
            output$UI_RemapSettings2  <- renderUI({
                tagList(
                    fluidRow(
                        column(11,
                            em(strong("Sample: "), sample_rawImagePath()),
                            br(),br(),
                            textInput("Settings_pathInTable", "Image root folder name in table", OA[["Settings_pathInTable"]], width = "100%"),
                            textInput("Settings_pathInComputer", "Image root folder name in this computer", OA[["Settings_pathInComputer"]], width = "100%")
                        ),
                        column(1,
                            actionButton("help_Settings_pathInTable", "", icon = icon("question-circle")),
                            actionButton("help_Settings_pathInComputer", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_pathInTable {margin-top: 65px;}"),
                        tags$style(type='text/css', "#help_Settings_pathInComputer {margin-top: 40px;}")
                    )
                )
            })
            output$UI_RemapSettings3  <- renderUI({
                res <- if(exists_remappedImagePath()){
                    strong(div("Found file!", style = "color:darkgreen"))
                } else{
                    strong(div("File not found.", style = "color:red"))
                }

                fluidRow(
                    column(11,
                        em(strong("Sample: "), sample_remappedImagePath()),
                        res
                    )
                )
            })
            output$UI_MasksSettings1  <- renderUI({
                tagList(
                    h3("Segmentation Masks Settings"),
                    p("Find the location of the segmentation masks generated by CellProfiler or Fiji."),
                    p(" "),
                    fluidRow(
                        column(11,
                            checkboxInput("Settings_generateMasks", label = "Generate segmentation masks?", value = OA[["Settings_generateMasks"]])
                        ),
                        column(1,
                            actionButton("help_Settings_generateMasks", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_generateMasks {margin-top: 5px;}")
                    )
                )
            })
            output$UI_MasksSettings2  <- renderUI({
                if(is.null(input$Settings_generateMasks)) return(NULL)
                if(!input$Settings_generateMasks) return(NULL)

                tagList(
                    fluidRow(
                        column(11,
                               br(),
                               em(strong("Sample raw image: "), sample_rawImagePath()),
                               br(),br(),br(),
                               textInput("Settings_MimagepathInTable", "Image root folder name in table", OA[["Settings_MimagepathInTable"]], width = "100%"),
                               textInput("Settings_MimagepathInComputer", "Image root folder name in this computer", OA[["Settings_MimagepathInComputer"]], width = "100%"),
                               numericInput("Settings_rawSuffixLength", "Length of image suffix", value = OA[["Settings_rawSuffixLength"]], min = 1, max = nchar(sample_remappedImagePath()))
                        ),
                        column(1,
                               actionButton("help_Settings_MimagepathInTable", "", icon = icon("question-circle")),
                               actionButton("help_Settings_MimagepathInComputer", "", icon = icon("question-circle")),
                               actionButton("help_Settings_rawSuffixLength", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_MimagepathInTable {margin-top: 106px;}"),
                        tags$style(type='text/css', "#help_Settings_MimagepathInComputer {margin-top: 40px;}"),
                        tags$style(type='text/css', "#help_Settings_rawSuffixLength {margin-top: 41px;}")
                    )
                )
            })
            output$UI_MasksSettings3  <- renderUI({
                if(is.null(input$Settings_generateMasks)) return(NULL)
                if(!input$Settings_generateMasks) return(NULL)

                tagList(
                    fluidRow(
                        column(11,
                               em(strong("Raw file suffix: "), sample_rawImageSuffix()),
                               br(),br(),br(),
                               textInput("Settings_maskFileSuffix", "Suffix for segmentation mask files", OA[["Settings_maskFileSuffix"]], width = "100%")
                        ),
                        column(1,
                               actionButton("help_Settings_maskFileSuffix", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_maskFileSuffix {margin-top: 85px;}")
                    )
                )
            })
            output$UI_MasksSettings4  <- renderUI({
                if(is.null(input$Settings_generateMasks)) return(NULL)
                if(!input$Settings_generateMasks) return(NULL)

                res <- if(exists_remappedMaskPath()){
                    strong(div("Found file!", style = "color:darkgreen"))
                } else{
                    strong(div("File not found.", style = "color:red"))
                }

                fluidRow(
                    column(11,
                           em(strong("Sample: "), sample_remappedMaskPath()),
                           res,
                           br(),br(),br()
                    )
                )
            })
            output$UI_MasksSettings5  <- renderUI({
                if(is.null(input$Settings_generateMasks)) return(NULL)
                if(!input$Settings_generateMasks) return(NULL)

                tagList(
                    fluidRow(
                        column(11,
                               textInput("Settings_OAMasksFileSuffix", "Suffix for Organoid Analyst masks file", OA[["Settings_OAMasksFileSuffix"]], width = "100%")
                        ),
                        column(1,
                               actionButton("help_Settings_OAMasksFileSuffix", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_OAMasksFileSuffix {margin-top: 0px;}")
                    )
                )
            })
            output$UI_MasksSettings6  <- renderUI({
                if(is.null(input$Settings_generateMasks)) return(NULL)
                if(!input$Settings_generateMasks) return(NULL)

                fluidRow(
                    column(11,
                           em(strong("Sample mask file: "), sample_newMaskPath()),
                           br(),br(),
                           textInput("Settings_OALabelsFileSuffix", "Suffix for Organoid Analyst labels file", OA[["Settings_OALabelsFileSuffix"]], width = "100%")
                    ),
                    column(1,
                           actionButton("help_Settings_OALabelsFileSuffix", "", icon = icon("question-circle"))
                    ),
                    tags$style(type='text/css', "#help_Settings_OALabelsFileSuffix {margin-top: 67px;}")
                )
            })
            output$UI_MasksSettings7  <- renderUI({
                if(is.null(input$Settings_generateMasks)) return(NULL)
                if(!input$Settings_generateMasks) return(NULL)

                fluidRow(
                    column(11,
                           em(strong("Sample labels file: "), sample_newLabelsPath())
                    )
                )
            })
            output$UI_ViewSettings1   <- renderUI({
                if (Sys.info()['sysname'] == "Windows"){
                    return({
                        tagList(
                            h3("Interaction with Fiji Settings"),
                            fluidRow(
                                column(11,
                                    textInput("fiji_binary", "Path to Fiji (only necessary for Windows)", value = OA[["fiji_binary"]], width = "100%")
                                ),
                                column(1,
                                    actionButton("help_fiji_binary", "", icon = icon("question-circle"))
                                ),
                                tags$style(type='text/css', "#help_fiji_binary {margin-top: 25px;}")
                            )
                        )
                    })
                }
                if (Sys.info()['sysname'] == "Darwin"){
                    return({
                        tagList(
                            h3("Fiji location"),
                            fluidRow(
                                column(11,
                                       br(),
                                       OA[["fiji_binary"]]
                                ),
                                column(1,
                                       actionButton("help_fiji_binary", "", icon = icon("question-circle"))
                                ),
                                tags$style(type='text/css', "#help_fiji_binary {margin-top: 25px;}")
                            )
                        )
                    })
                }

                # If operating system is not Windows or MacOS X
                NULL
            })
            output$UI_ViewSettings2   <- renderUI({

                input$fiji_binary
                if(Sys.info()['sysname'] %in% c("Windows", "Darwin")){

                    if(file.exists(OA[["fiji_binary"]])){
                        res <- strong(div("Found Fiji!", style = "color:darkgreen"))
                    } else{
                        res <- strong(div("Fiji not found. Image visualization will be disabled.", style = "color:red"))
                    }

                } else{
                    res <- strong(div("Fiji is not supported in this system."))
                }


                fluidRow(
                    column(11,
                        res
                    )
                )
            })
            output$UI_ButtonNormalize <- renderUI({
                tagList(
                    fluidRow(
                        column(width = 11,
                            actionButton("ButtonNormalize", "Normalize data", icon = icon("paper-plane-o"))
                        ),
                        column(width = 1,
                            actionButton("help_ButtonNormalize", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_ButtonNormalize {margin-top: 0px;}")
                    )
                )
            })

        }
    })


    ### Change output folder
    observeEvent(input$Settings_folderOutput, {

        # Make sure to respond only if the user has selcted a folder
        if(!is.integer(input$Settings_folderOutput)){

            OA[["outputfolder"]] <<- parseDirPath(volumes, input$Settings_folderOutput)

            output$UI_outputFolder <- renderUI({
                tagList(
                    h3("Output Folder"),
                    fluidRow(
                        column(11,
                            shinyDirButton("Settings_folderOutput", "Change output folder...", "Select the folder to keep analysis results", icon = icon("folder")),
                            br(),
                            br(),
                            em(strong("Output folder: "), OA$outputfolder)
                        ),
                        column(1,
                            actionButton("help_Settings_folderOutput", "", icon = icon("question-circle"))
                        ),
                        tags$style(type='text/css', "#help_Settings_folderOutput {margin-top: 0px;}")
                    )
                )
            })
        }

    })



    # 2. Data normalization ------------------------------------

    # Data Normalization
    observeEvent(input$ButtonNormalize, {

        withCallingHandlers({
            html("echo_norm", "", add = FALSE)


            ### Remap file locations
            # Raw images
            pathInTable                           <- gsub("\\\\", "/", OA[["Settings_pathInTable"]])
            pathInTable                           <- sub("^file:///", "", pathInTable)
            pathInComputer                        <- gsub("\\\\", "/", OA[["Settings_pathInComputer"]])
            pathInComputer                        <- sub("^file:///", "", pathInComputer)
            OA[["dataset.raw"]]$OA_path_rawimage <<- OA[["dataset.raw"]][[OA[["Settings_colRawImgPath"]]]]
            OA[["dataset.raw"]]$OA_path_rawimage <<- sub("^file:///", "", OA[["dataset.raw"]]$OA_path_rawimage)
            OA[["dataset.raw"]]$OA_path_rawimage <<- gsub("\\\\", "/", OA[["dataset.raw"]]$OA_path_rawimage)
            OA[["dataset.raw"]]$OA_path_rawimage <<- gsub(pathInTable,
                                                          pathInComputer,
                                                          OA[["dataset.raw"]]$OA_path_rawimage)

            # Raw masks
            if(OA[["Settings_generateMasks"]]){

                pathInTable                          <- gsub("\\\\", "/", OA[["Settings_MimagepathInTable"]])
                pathInTable                          <- sub("^file:///", "", pathInTable)
                pathInComputer                       <- gsub("\\\\", "/", OA[["Settings_MimagepathInComputer"]])
                pathInComputer                       <- sub("^file:///", "", pathInComputer)
                maskSuffix                           <- OA[["Settings_maskFileSuffix"]]
                OA[["dataset.raw"]]$OA_path_rawmask <<- OA[["dataset.raw"]][[OA[["Settings_colRawImgPath"]]]]
                OA[["dataset.raw"]]$OA_path_rawmask <<- gsub("\\\\", "/", OA[["dataset.raw"]]$OA_path_rawmask)
                OA[["dataset.raw"]]$OA_path_rawmask <<- sub("^file:///", "", OA[["dataset.raw"]]$OA_path_rawmask)
                OA[["dataset.raw"]]$OA_path_rawmask <<- sub(pathInTable, pathInComputer, OA[["dataset.raw"]]$OA_path_rawmask)


                OA[["dataset.raw"]]$OA_path_rawmask <<- sapply(OA[["dataset.raw"]]$OA_path_rawmask, function(x){
                                                               rawSuffix <- substring(x, nchar(x) - OA[["Settings_rawSuffixLength"]] + 1, nchar(x))
                                                               sub(paste0(rawSuffix, "$"), maskSuffix, x)
                                                        })
            } else{
                OA[["dataset.raw"]]$OA_path_rawmask <<- NA
            }


            # Organoid Analyst masks
            if(OA[["Settings_generateMasks"]]){

                oldSuffix                           <- OA[["Settings_maskFileSuffix"]]
                newSuffix                           <- paste0(OA[["Settings_OAMasksFileSuffix"]], ".png")
                OA[["dataset.raw"]]$OA_path_OAmask <<- sub(OA[["sourcefolder"]], OA[["outputfolder"]], OA[["dataset.raw"]]$OA_path_rawmask)
                OA[["dataset.raw"]]$OA_path_OAmask <<- sub(paste0(oldSuffix, "$"), newSuffix, OA[["dataset.raw"]]$OA_path_OAmask)
            } else{
                OA[["dataset.raw"]]$OA_path_OAmask <<- NA
            }

            # Organoid Analyst labels
            if(OA[["Settings_generateMasks"]]){

                oldSuffix                            <- OA[["Settings_maskFileSuffix"]]
                newSuffix                            <- paste0(OA[["Settings_OALabelsFileSuffix"]], ".png")
                OA[["dataset.raw"]]$OA_path_OAlabel <<- sub(OA[["sourcefolder"]], OA[["outputfolder"]], OA[["dataset.raw"]]$OA_path_rawmask)
                OA[["dataset.raw"]]$OA_path_OAlabel <<- sub(paste0(oldSuffix, "$"), newSuffix, OA[["dataset.raw"]]$OA_path_OAlabel)
            } else{
                OA[["dataset.raw"]]$OA_path_OAlabel <<- NA
            }




            # Pre-normalization sanity checks
            {
                proceed_with_normalization <- TRUE
                # 1. colWell: data type
                if(!is.integer(OA[["dataset.raw"]][[OA[["Settings_colWell"]]]])){
                    message("Error: Well numbers are not integer values. Normalization aborted.")
                    showModal(modalDialog(title = "Error: WELL values", "Well numbers are not integer values. Check raw values and Organoid Analyst settings. Normalization aborted."))
                    proceed_with_normalization <- FALSE
                }
                # 2. colTime: data type
                if(!is.integer(OA[["dataset.raw"]][[OA[["Settings_colTime"]]]])){
                    message("Error: Time numbers are not integer values. Normalization aborted.")
                    showModal(modalDialog(title = "Error: TIME values", "Time numbers are not integer values. Check raw values and Organoid Analyst settings. Normalization aborted."))
                    proceed_with_normalization <- FALSE
                }
                # 3. colArea: data type
                if(!is.numeric(OA[["dataset.raw"]][[OA[["Settings_colArea"]]]])){
                    message("Error: Area values are not in numeric format. Normalization aborted.")
                    showModal(modalDialog(title = "Error: AREA values", "Area values are not numeric. Check raw values and Organoid Analyst settings. Normalization aborted."))
                    proceed_with_normalization <- FALSE
                }
                # 4. slider_tf & slider_initial: realistic value
                if(proceed_with_normalization){
                    all_time_values        <- unique(OA[["dataset.raw"]][[OA[["Settings_colTime"]]]] * OA[["Settings_TimeRes"]])
                    all_time_values        <- all_time_values[!is.na(all_time_values)] 
                    OA[["slider_tf"]]      <- if(60 %in% all_time_values){
                                                  60
                                              } else{
                                                  max(all_time_values)
                                              }
                    OA[["slider_initial"]] <- if(all(c(10, 30) %in% all_time_values)){
                                                  c(10, 30) 
                                              }else{
                                                  c(min(all_time_values), max(all_time_values))
                                              }
                }
            }




            # Normalize data
            if(proceed_with_normalization){


                message("Starting data normalization")
                message("")

                OA[["dataset.norm"]] <- normalizeFIS(df           = OA[["dataset.raw"]],
                                                     colWell      = OA[["Settings_colWell"]],
                                                     colTimePoint = OA[["Settings_colTime"]],
                                                     colArea      = OA[["Settings_colArea"]],
                                                     colCompound  = OA[["Settings_colCompound"]],
                                                     colConc      = OA[["Settings_colConcentration"]],
                                                     colID        = OA[["Settings_colOrganoidID"]],
                                                     nameInvalid  = OA[["Settings_nameInvalid"]],
                                                     numRows      = OA[["Settings_numRows"]],
                                                     numCols      = OA[["Settings_numCols"]],
                                                     timeRes      = OA[["Settings_TimeRes"]])
                OA[["dataset.norm"]] <- addslopesFIS(df       = OA[["dataset.norm"]],
                                                     colWell  = "well",
                                                     colTime  = "time",
                                                     colData  = "normalized",
                                                     t0       = OA[["slider_initial"]][1],
                                                     tf       = OA[["slider_initial"]][2],
                                                     colSlope = OA[["colInitSlope"]])
                OA[["dataset.norm.QC"]] <- OA[["dataset.norm"]]
                message("")
                message("Finished data normalization")
                message("")
                
                
                # Render results to be visible by the user
                output$table_normalized <- DT::renderDataTable(OA[["dataset.norm"]])
                message("Normalized values can be viewed in 'View Data > Normalized data'")
                message("")
                
                
                # Generate segmentation masks
                if(OA[["Settings_generateMasks"]]){
                    message("Generating segmentation masks. This may take a few minutes...")
                    makeSegmentationMasks(df                 = OA[["dataset.raw"]],
                                          colPath_oldmasks   = "OA_path_rawmask",
                                          colPath_newmasks   = "OA_path_OAmask",
                                          colPath_newlabels  = "OA_path_OAlabel",
                                          colX               = OA[["Settings_colX"]],
                                          colY               = OA[["Settings_colY"]],
                                          colLabel           = OA[["Settings_colOrganoidID"]],
                                          discardTheseLabels = OA[["Settings_nameInvalid"]])
                    message("")
                    message("Finished generating segmentation masks.")
                    message("")
                }
                
                
                # Navigate to next tab
                updateNavbarPage(session, "mainNavbarPage", selected = "tabPlotting")
                
            }
            OA <<- OA

        },
            message = function(m) html("echo_norm", m$message, add = TRUE)
        )


        # Initialize plotting datasets:

        # 0.1. Normalized data after QC
        OA[["dataset.norm.QC"]] <<- OA[["dataset.norm"]]


        # 0.2. Subset data (pick final data point only). This does not interfere with initial slope calculation
        tf   <- ifelse(is.null(isolate(input$slider_tf)),
                       OA[["slider_tf"]],
                       isolate(input$slider_tf))
        resultsFIS_tf <- OA[["dataset.norm.QC"]]
        resultsFIS_tf <- resultsFIS_tf[!is.null(resultsFIS_tf$time),]
        resultsFIS_tf <- resultsFIS_tf[resultsFIS_tf$time == tf,]

        # 1. Initial slope summary
        temp.slope <- by(resultsFIS_tf, resultsFIS_tf$treatment, function(x){
            data.frame(treatment      = unique(x$treatment),
                       compound       = unique(x$compound),
                       concentration  = unique(x$concentration),
                       mean           = mean(x$initialSlope, na.rm = TRUE),
                       sd             = sd(x$initialSlope, na.rm = TRUE),
                       n              = sum(!is.na(x$initialSlope)),
                       SEM            = sd(x$initialSlope, na.rm = TRUE) / sqrt(sum(!is.na(x$initialSlope))),
                       stringsAsFactors = FALSE)
        })
        OA[["summary.slope"]] <- do.call("rbind", temp.slope)

        # 2. AUC summary
        temp.auc  <- by(resultsFIS_tf, resultsFIS_tf$treatment, function(x){
            data.frame(treatment        = unique(x$treatment),
                       compound         = unique(x$compound),
                       concentration    = unique(x$concentration),
                       mean             = mean(x$cumulative_AUC, na.rm = TRUE),
                       sd               = sd(x$cumulative_AUC, na.rm = TRUE),
                       n                = sum(!is.na(x$cumulative_AUC)),
                       SEM              = sd(x$cumulative_AUC, na.rm = TRUE) / sqrt(sum(!is.na(x$cumulative_AUC))),
                       stringsAsFactors = FALSE)
        })
        OA[["summary.auc"]] <- do.call("rbind", temp.auc)

        # 3. Area fold change
        temp.areachenge <- by(resultsFIS_tf, resultsFIS_tf$treatment, function(x){
            data.frame(treatment      = unique(x$treatment),
                       compound       = unique(x$compound),
                       concentration  = unique(x$concentration),
                       mean           = mean(x$normalized, na.rm = TRUE),
                       sd             = sd(x$normalized, na.rm = TRUE),
                       n              = sum(!is.na(x$normalized)),
                       SEM            = sd(x$normalized, na.rm = TRUE) / sqrt(sum(!is.na(x$normalized))),
                       stringsAsFactors = FALSE)
        })
        OA[["summary.areachange"]] <- do.call("rbind", temp.areachenge)



        # Generate UI for plotting
        output$UI_plotting               <- renderUI({
            tagList(
                fixedRow(
                    column(width = 10,
                        sliderInput("slider_initial",
                                    label = "Select initial data points",
                                    min = min(OA[["dataset.norm"]]$time),
                                    max = max(OA[["dataset.norm"]]$time),
                                    value = c(min(OA[["dataset.norm"]]$time), max(OA[["dataset.norm"]]$time)),
                                    step = OA[["Settings_TimeRes"]])
                    ),
                    column(width = 1,
                        actionButton("help_Plotting_initialTimePoints", "", icon = icon("question-circle"))
                    ),
                    tags$style(type='text/css', "#help_Plotting_initialTimePoints {margin-top: 38px;}")
                ),
                fluidRow(
                    column(width = 10,
                        sliderInput("slider_tf",
                                    label = "Select final experiment time",
                                    min = min(OA[["dataset.norm"]]$time),
                                    max = max(OA[["dataset.norm"]]$time),
                                    value = max(OA[["dataset.norm"]]$time),
                                    step = OA[["Settings_TimeRes"]])
                    ),
                    column(width = 1,
                        actionButton("help_Plotting_finalExperimentTime", "", icon = icon("question-circle"))
                    ),
                    tags$style(type='text/css', "#help_Plotting_finalExperimentTime {margin-top: 38px;}")

                )
            )
        })
        output$plotPlate                 <- renderUI({
            input$p1_height
            plotOutput("plot_plate", height = OA[["p1_height"]])
        })
        output$UI_options_plotPlate      <- renderUI({
            wellPanel(
                h2("Swelling kinetics for all plate wells"),
                fluidRow(
                    column(width = 4,
                        sliderInput("p1_height", "Plot height", min = 200, max = 1500, value = OA[["p1_height"]], step = 100)
                    ),
                    column(width = 1,
                        NULL
                    ),
                    column(width = 3,
                        checkboxInput("p1_showFit", "Show initial fit?", value = OA[["p1_showFit"]], width = "100%"),
                        checkboxInput("p1_showLUT", "Color by swelling rate?", value = OA[["p1_showLUT"]], width = "100%")
                    ),
                    column(width = 4,
                        selectInput("p1_highlight", "Highlight wells by:", choices = as.list(c("None", "treatment", "compound", "concentration")), selected = OA[["p1_highlight"]], width = "100%"),
                        numericInput("p1_labelSize", "Label size", value = OA[["p1_labelSize"]], width = "100%")
                    )
                )
            )
        })
        output$UI_options_plotTitration  <- renderUI({
            wellPanel(
                fluidRow(
                    column(width = 12,
                        selectInput("p2_selectTreatment", "Select compound to show", choices = as.list(c("All", unique(OA[["summary.auc"]]$compound))), selected = OA[["p2_selectTreatment"]], multiple = TRUE)
                    )
                )
            )
        })
        output$UI_options_plotAUC        <- renderUI({
            wellPanel(
                fluidRow(
                    column(width = 6,
                        selectInput("p3_selectTreatment", "Select compound to show", choices = as.list(c("All", unique(OA[["summary.auc"]]$compound))), selected = "All", multiple = TRUE)
                    ),
                    column(width = 6,
                        checkboxInput("p3_showLUT", "Color by compound?", value = FALSE, width = "100%")
                    )
                )
            )
        })
        output$UI_options_plotSlopes     <- renderUI({
            wellPanel(
                fluidRow(
                    column(width = 6,
                           selectInput("p4_selectTreatment", "Select compound to show", choices = as.list(c("All", unique(OA[["summary.slope"]]$compound))), selected = "All", multiple = TRUE)
                    ),
                    column(width = 6,
                           checkboxInput("p4_showLUT", "Color by compound?", value = FALSE, width = "100%")
                    )
                )
            )
        })
        output$UI_options_plotNormalized <- renderUI({
            wellPanel(
                fluidRow(
                    column(width = 6,
                           selectInput("p5_selectTreatment", "Select compound to show", choices = as.list(c("All", unique(OA[["summary.areachange"]]$compound))), selected = "All", multiple = TRUE)
                    ),
                    column(width = 6,
                           checkboxInput("p5_showLUT", "Color by compound?", value = FALSE, width = "100%")
                    )
                )
            )
        })
        output$UI_QClist                 <- renderUI({
            fluidRow(
                column(width = 10,
                    selectInput("QClist", "Select wells you wish to exclude from calculations", choices = as.list(unique(OA[["dataset.raw"]][[OA[["Settings_colWell"]]]])), selected = OA[["QClist"]], multiple = TRUE)
                ),
                column(width = 1,
                    actionButton("help_QClist", "", icon = icon("question-circle"))
                ),
                tags$style(type='text/css', "#help_QClist {margin-top: 45px;}")
            )
        })
        output$UI_openFiji               <- renderUI({
            tagList(
                fluidRow(
                    column(width = 12,
                        selectInput("targetWells", "Select wells to be opened in Fiji", choices = as.list(unique(OA[["dataset.raw"]][[OA[["Settings_colWell"]]]])), multiple = TRUE, width = "100%")
                    )
                ),
                fluidRow(
                    column(width = 10,
                        actionButton("buttonOpenInFiji", "Open movies in Fiji", icon = icon("film"), width = "100%")
                    ),
                    column(width = 2,
                        actionButton("help_fijiWells", "", icon = icon("question-circle"))
                    ),
                    tags$style(type='text/css', "#help_fijiWells {margin-top: 0px;}")
                )
            )
        })
        output$UI_ExportFinal            <- renderUI({
            input$Settings_folderOutput

            tagList(
                fluidRow(
                    column(width = 2,
                           actionButton("ExportFinal", "Export data", icon = icon("paper-plane-o"), width = "100%")
                    ),
                    column(width = 9,
                           renderPrint(paste0("Output folder: ", OA[["outputfolder"]]))
                    ),
                    column(width = 1,
                           actionButton("help_exportFinal", "", icon = icon("question-circle"))
                    ),
                    tags$style(type='text/css', "#help_exportFinal {margin-top: 0px;}")
                )
            )
        })
    })



    # 3. Plotting ------------------------------------

    # Update plotting datasets
    observeEvent(input$slider_initial, {

        # Update initial slope values
        OA[["dataset.norm"]] <<- addslopesFIS(df       = OA[["dataset.norm"]],
                                              colWell  = "well",
                                              colTime  = "time",
                                              colData  = "normalized",
                                              t0       = OA[["slider_initial"]][1],
                                              tf       = OA[["slider_initial"]][2],
                                              colSlope = OA[["colInitSlope"]])

        # Update well QC
        OA[["dataset.norm.QC"]] <<- OA[["dataset.norm"]]
        OA[["dataset.norm.QC"]][OA[["dataset.norm.QC"]]$well %in% OA[["QClist"]], c("sumarea", "normalized", "normalized_offset", "cumulative_AUC", OA[["colInitSlope"]])] <<- NA

        # Refresh data table display
        output$table_normalized <- DT::renderDataTable(OA[["dataset.norm"]])

    })

    output$plot_plate      <- renderPlot({
        input$QClist
        input$slider_initial
        input$slider_tf
        input$p1_highlight
        input$p1_showFit
        input$p1_showLUT
        input$p1_labelSize

        ## Update well QC

        # Exclude undesired wells
        OA[["dataset.norm.QC"]] <<- OA[["dataset.norm"]]
        OA[["dataset.norm.QC"]][OA[["dataset.norm.QC"]]$well %in% isolate(input$QClist), c("sumarea", "normalized", "normalized_offset", "cumulative_AUC", OA[["colInitSlope"]])] <<- NA

        # kinetics for all wells
        g_plate <<- plot_plate(df           = OA[["dataset.norm.QC"]],
                               slope_xmin   = input$slider_initial[1],
                               slope_xmax   = input$slider_initial[2],
                               colX         = "time",
                               colY         = "normalized",
                               colWell      = "well",
                               colSlope     = "initialSlope",
                               numrows      = OA[["Settings_numRows"]],
                               numcols      = OA[["Settings_numCols"]],
                               xmin         = NA,
                               xmax         = NA,
                               ymin         = NA,
                               ymax         = NA,
                               title        = paste0(OA[["sourcefolder"]], " [", OA[["slider_tf"]], "min]"),
                               xlab         = "Time (min)",
                               ylab         = "Normalized area (%)",
                               colHighlight = input$p1_highlight,
                               showFit      = input$p1_showFit,
                               showFitLUT   = input$p1_showLUT,
                               labelsize    = input$p1_labelSize)
        g_plate

    })
    output$plot_titration  <- renderPlot({
        input$QClist
        input$slider_tf
        input$p2_selectTreatment

        ## Update well QC

        # Exclude undesired wells
        OA[["dataset.norm.QC"]] <- OA[["dataset.norm"]]
        OA[["dataset.norm.QC"]][OA[["dataset.norm.QC"]]$well %in% isolate(input$QClist), c("sumarea", "normalized", "normalized_offset", "cumulative_AUC", OA[["colInitSlope"]])] <- NA


        # Subset data:pick final data point only
        tf   <- ifelse(is.null(isolate(input$slider_tf)),
                       OA[["slider_tf"]],
                       isolate(input$slider_tf))
        resultsFIS_tf <- OA[["dataset.norm.QC"]]
        resultsFIS_tf <- resultsFIS_tf[!is.null(resultsFIS_tf$time),]
        resultsFIS_tf <- resultsFIS_tf[resultsFIS_tf$time == tf,]

        # Compute summary data frame
        temp.auc  <- by(resultsFIS_tf, resultsFIS_tf$treatment, function(x){
            data.frame(treatment        = unique(x$treatment),
                       compound         = unique(x$compound),
                       concentration    = unique(x$concentration),
                       mean             = mean(x$cumulative_AUC, na.rm = TRUE),
                       sd               = sd(x$cumulative_AUC, na.rm = TRUE),
                       n                = sum(!is.na(x$cumulative_AUC)),
                       SEM              = sd(x$cumulative_AUC, na.rm = TRUE) / sqrt(sum(!is.na(x$cumulative_AUC))),
                       stringsAsFactors = FALSE)
        })
        OA[["summary.auc"]] <- do.call("rbind", temp.auc)
        OA[["summary.auc"]] <<- OA[["summary.auc"]]


        ## Titration plot
        g_tit <<- plot_titration(df                = OA[["summary.auc"]],
                                 colX              = "concentration",
                                 colY              = "mean",
                                 colsdY            = "sd",
                                 colTreatment      = "compound",
                                 pickThisTreatment = ifelse(is.null(isolate(input$p2_selectTreatment)),
                                                            OA[["p2_selectTreatment"]],
                                                            isolate(input$p2_selectTreatment)),
                                 categoricalX      = TRUE,
                                 xlab              = "Compound Concentration",
                                 ylab              = "AUC (mean ± sd)",
                                 axistitlesize     = 20,
                                 axislabelsize     = 15)
        g_tit
    })
    output$plot_AUC        <- renderPlot({
        input$QClist
        input$slider_tf
        input$p3_selectTreatment
        input$p3_showLUT

        ## Update well QC

        # Exclude undesired wells
        OA[["dataset.norm.QC"]] <- OA[["dataset.norm"]]
        OA[["dataset.norm.QC"]][OA[["dataset.norm.QC"]]$well %in% isolate(input$QClist), c("sumarea", "normalized", "normalized_offset", "cumulative_AUC", OA[["colInitSlope"]])] <- NA


        # Subset data:pick final data point only
        tf   <- ifelse(is.null(isolate(input$slider_tf)),
                       OA[["slider_tf"]],
                       isolate(input$slider_tf))
        resultsFIS_tf <- OA[["dataset.norm.QC"]]
        resultsFIS_tf <- resultsFIS_tf[!is.null(resultsFIS_tf$time),]
        resultsFIS_tf <- resultsFIS_tf[resultsFIS_tf$time == tf,]

        # Compute summary data frame
        temp.auc  <- by(resultsFIS_tf, resultsFIS_tf$treatment, function(x){
            data.frame(treatment        = unique(x$treatment),
                       compound         = unique(x$compound),
                       concentration    = unique(x$concentration),
                       mean             = mean(x$cumulative_AUC, na.rm = TRUE),
                       sd               = sd(x$cumulative_AUC, na.rm = TRUE),
                       n                = sum(!is.na(x$cumulative_AUC)),
                       SEM              = sd(x$cumulative_AUC, na.rm = TRUE) / sqrt(sum(!is.na(x$cumulative_AUC))),
                       stringsAsFactors = FALSE)
        })
        OA[["summary.auc"]] <- do.call("rbind", temp.auc)
        OA[["summary.auc"]] <<- OA[["summary.auc"]]


        ## Bar chart with error bars
        g_AUC <<- plot_AUC(df                = OA[["summary.auc"]],
                           colX              = "treatment",
                           colY              = "mean",
                           colsdY            = "sd",
                           title             = paste0(OA[["sourcefolder"]], " [", OA[["slider_tf"]], "min]"),
                           xlab              = NULL,
                           ylab              = "AUC (mean ± sd)",
                           axistitlesize     = 20,
                           axislabelsize     = 12,
                           colTreatment      = "compound",
                           pickThisTreatment = ifelse(is.null(isolate(input$p3_selectTreatment)),
                                                      OA[["p3_selectTreatment"]],
                                                      isolate(input$p3_selectTreatment)),
                           colorbyTreatment  = ifelse(is.null(isolate(input$p3_showLUT)),
                                                      OA[["p3_showLUT"]],
                                                      isolate(input$p3_showLUT)))
        g_AUC
    })
    output$plot_slopes     <- renderPlot({
        input$QClist
        input$slider_initial
        input$p4_selectTreatment
        input$p4_showLUT

        ## Update well QC

        # Exclude undesired wells
        OA[["dataset.norm.QC"]] <- OA[["dataset.norm"]]
        OA[["dataset.norm.QC"]][OA[["dataset.norm.QC"]]$well %in% isolate(input$QClist), c("sumarea", "normalized", "normalized_offset", "cumulative_AUC", OA[["colInitSlope"]])] <- NA


        # Subset data:pick final data point only
        tf   <- ifelse(is.null(isolate(input$slider_tf)),
                       OA[["slider_tf"]],
                       isolate(input$slider_tf))
        resultsFIS_tf <- OA[["dataset.norm.QC"]]
        resultsFIS_tf <- resultsFIS_tf[!is.null(resultsFIS_tf$time),]
        resultsFIS_tf <- resultsFIS_tf[resultsFIS_tf$time == tf,]

        # Compute summary data frame
        temp.slope <- by(resultsFIS_tf, resultsFIS_tf$treatment, function(x){
            data.frame(treatment      = unique(x$treatment),
                       compound       = unique(x$compound),
                       concentration  = unique(x$concentration),
                       mean           = mean(x$initialSlope, na.rm = TRUE),
                       sd             = sd(x$initialSlope, na.rm = TRUE),
                       n              = sum(!is.na(x$initialSlope)),
                       SEM            = sd(x$initialSlope, na.rm = TRUE) / sqrt(sum(!is.na(x$initialSlope))),
                       stringsAsFactors = FALSE)
        })
        OA[["summary.slope"]] <- do.call("rbind", temp.slope)
        OA[["summary.slope"]] <<- OA[["summary.slope"]]


        ## Bar chart with error bars
        g_slopes <<- plot_slopes(df                = OA[["summary.slope"]],
                                 colX              = "treatment",
                                 colY              = "mean",
                                 colsdY            = "sd",
                                 title             = paste0(OA[["sourcefolder"]], " [", OA[["slider_tf"]], "min]"),
                                 xlab              = NULL,
                                 ylab              = "Initial swelling rate (mean ± sd)",
                                 axistitlesize     = 20,
                                 axislabelsize     = 12,
                                 colTreatment      = "compound",
                                 pickThisTreatment = ifelse(is.null(isolate(input$p4_selectTreatment)),
                                                            OA[["p4_selectTreatment"]],
                                                            isolate(input$p4_selectTreatment)),
                                 colorbyTreatment  = ifelse(is.null(isolate(input$p4_showLUT)),
                                                            OA[["p4_showLUT"]],
                                                            isolate(input$p4_showLUT)))
        g_slopes
    })
    output$plot_normalized <- renderPlot({
        input$QClist
        input$slider_tf
        input$p5_selectTreatment
        input$p5_showLUT

        ## Update well QC

        # Exclude undesired wells
        OA[["dataset.norm.QC"]] <- OA[["dataset.norm"]]
        OA[["dataset.norm.QC"]][OA[["dataset.norm.QC"]]$well %in% isolate(input$QClist), c("sumarea", "normalized", "normalized_offset", "cumulative_AUC", OA[["colInitSlope"]])] <- NA


        # Subset data: pick final data point only
        tf   <- ifelse(is.null(isolate(input$slider_tf)),
                       OA[["slider_tf"]],
                       isolate(input$slider_tf))
        resultsFIS_tf <- OA[["dataset.norm.QC"]]
        resultsFIS_tf <- resultsFIS_tf[!is.null(resultsFIS_tf$time),]
        resultsFIS_tf <- resultsFIS_tf[resultsFIS_tf$time == tf,]

        # Compute summary data frame
        temp.areachange <- by(resultsFIS_tf, resultsFIS_tf$treatment, function(x){
            data.frame(treatment      = unique(x$treatment),
                       compound       = unique(x$compound),
                       concentration  = unique(x$concentration),
                       mean           = mean(x$normalized, na.rm = TRUE),
                       sd             = sd(x$normalized, na.rm = TRUE),
                       n              = sum(!is.na(x$normalized)),
                       SEM            = sd(x$normalized, na.rm = TRUE) / sqrt(sum(!is.na(x$normalized))),
                       stringsAsFactors = FALSE)
        })
        OA[["summary.areachange"]] <- do.call("rbind", temp.areachange)
        OA[["summary.areachange"]] <<- OA[["summary.areachange"]]


        ## Bar chart with error bars
        g_norm <<- plot_normalized(df                = OA[["summary.areachange"]],
                                   colX              = "treatment",
                                   colY              = "mean",
                                   colsdY            = "sd",
                                   title             = paste0(OA[["sourcefolder"]], " [", OA[["slider_tf"]], "min]"),
                                   xlab              = NULL,
                                   ylab              = "Normalized Area (mean ± sd)",
                                   axistitlesize     = 20,
                                   axislabelsize     = 12,
                                   colTreatment      = "compound",
                                   pickThisTreatment = ifelse(is.null(isolate(input$p5_selectTreatment)),
                                                              OA[["p5_selectTreatment"]],
                                                              isolate(input$p5_selectTreatment)),
                                   colorbyTreatment  = ifelse(is.null(isolate(input$p5_showLUT)),
                                                              OA[["p5_showLUT"]],
                                                              isolate(input$p5_showLUT)))
        g_norm
    })


    # Plot-Fiji interaction
    output$selection <- renderPrint({

        # Take dependency on clicking these buttons
        input$buttonOpenInFiji

        targetWells <- isolate(input$targetWells)


        withCallingHandlers({
            html("selection", "", add = FALSE)

            if(length(targetWells) == 0){
                message("Select a well to open images!")
            } else {
                message("You selected these wells:")
                message_wells <- paste(targetWells, collapse = ", ")
                message_wells <- gsub("^(.*), ", "\\1", message_wells)      # Remove trailing ', '
                message(message_wells)

                message("Preparing timelapse visualization")

                # Determine which LUTs to use to open images
                IJ_LUTs <- if(file.exists(paste0(dirname(OA[["fiji_binary"]]), "/luts/CellProfiler.lut"))){
                    c("Green", "CellProfiler", "Grays")
                } else{
                    c("Green", "glasbey_inverted", "Grays")
                }


                if(input$Settings_generateMasks){

                    # Open 3 image types: raw image, masks and labels
                    macrofile <- FIJI_openOrganoidImages(df           = OA[["dataset.raw"]],
                                                         targetWells  = targetWells,
                                                         colWell      = OA[["Settings_colWell"]],
                                                         colsFilePath = c("OA_path_rawimage", "OA_path_OAmask", "OA_path_OAlabel"),
                                                         LUTs         = IJ_LUTs,
                                                         fileSuffixes = c(gsub(".*(\\..+?$)", "\\1", OA[["dataset.raw"]][1, "OA_path_rawimage"]), input$Settings_OAMasksFileSuffix, input$Settings_OALabelsFileSuffix),    # regular expression returns the file extension
                                                         timeInterval = OA[["Settings_TimeRes"]])
                } else{

                    # Open raw images only
                    macrofile <- FIJI_openOrganoidImages(df           = OA[["dataset.raw"]],
                                                         targetWells  = targetWells,
                                                         colWell      = OA[["Settings_colWell"]],
                                                         colsFilePath = c("OA_path_rawimage"),
                                                         LUTs         = IJ_LUTs[3],
                                                         fileSuffixes = c("[]"))
                }
                # message(macrofile)

                # Windows command line command to start Fiji and the macro
                cmd <- paste0(OA[["fiji_binary"]], " --run ", macrofile)

                message("Launching Fiji now....")
                message(paste0("Running image open macro from ", macrofile))
                system(cmd, wait = FALSE)
            }

        },
            message = function(m) html("selection", m$message, add = TRUE)
        )

    })


    # 5. Export results ------------------------------------
    observeEvent(input$ExportFinal, {


        withCallingHandlers({
            html("echo_export", "", add = FALSE)
            message("Starting data export...")
            message("")

            if(!dir.exists(OA[["outputfolder"]])){
                dir.create(OA[["outputfolder"]], recursive = TRUE)
            }


            ## 5.1. Raw data ------------------------------------

            message("    Starting export of raw data")

            xlsxfile_raw <- paste0(OA[["outputfolder"]], "/FIS_rawdata.csv")
            message(paste0("        Saving normalized data to ", xlsxfile_raw))
            write.csv(OA[["dataset.raw"]], xlsxfile_raw, row.names = FALSE)
            message("    Saved raw data!")
            message("")



            ## 5.2. Normalized data ------------------------------------

            message("    Starting export of normalized data")

            # Prepare data for export
            Export_sumarea           <- as.data.frame(reshapeFIS(OA[["dataset.norm"]], "well", "time", "sumarea",           "compound", "concentration", "treatment"))
            Export_normalized        <- as.data.frame(reshapeFIS(OA[["dataset.norm"]], "well", "time", "normalized",        "compound", "concentration", "treatment"))
            Export_normalized_offset <- as.data.frame(reshapeFIS(OA[["dataset.norm"]], "well", "time", "normalized_offset", "compound", "concentration", "treatment"))
            Export_cumulative_AUC    <- as.data.frame(reshapeFIS(OA[["dataset.norm"]], "well", "time", "cumulative_AUC",    "compound", "concentration", "treatment"))
            Export_ISR               <- as.data.frame(reshapeFIS(OA[["dataset.norm"]], "well", "time", "initialSlope",      "compound", "concentration", "treatment"))[1:5,]

            # Create workbook and sheets
            myWorkbook              <- createWorkbook()
            sheet_sumarea           <- createSheet(wb = myWorkbook, sheetName = "sumarea")
            sheet_normalized        <- createSheet(wb = myWorkbook, sheetName = "normalized")
            sheet_normalized_offset <- createSheet(wb = myWorkbook, sheetName = "normalized_offset")
            sheet_cumulative_AUC    <- createSheet(wb = myWorkbook, sheetName = "cumulative_AUC")
            sheet_ISR               <- createSheet(wb = myWorkbook, sheetName = "initial_swelling_rate")

            # Add data to workbook
            addDataFrame(x = Export_sumarea,           sheet = sheet_sumarea,           startRow = 1, col.names = FALSE)
            addDataFrame(x = Export_normalized,        sheet = sheet_normalized,        startRow = 1, col.names = FALSE)
            addDataFrame(x = Export_normalized_offset, sheet = sheet_normalized_offset, startRow = 1, col.names = FALSE)
            addDataFrame(x = Export_cumulative_AUC,    sheet = sheet_cumulative_AUC,    startRow = 1, col.names = FALSE)
            addDataFrame(x = Export_ISR,               sheet = sheet_ISR,               startRow = 1, col.names = FALSE)

            # Save workbok
            xlsxfile_norm <- paste0(OA[["outputfolder"]], "/FIS_normalized.xlsx")
            message(paste0("        Saving normalized data to ", xlsxfile_norm))
            saveWorkbook(myWorkbook, xlsxfile_norm)
            message("    Saved normalized data!")
            message("")



            ## 5.3. Summarized data ------------------------------------

            message("    Starting export of summarized data")

            # Create workbook and sheets
            myWorkbook   <- createWorkbook()
            sheet_AUC    <- createSheet(wb = myWorkbook, sheetName = "AUC")
            sheet_slopes <- createSheet(wb = myWorkbook, sheetName = "initial_swelling_rate")
            sheet_AtA0   <- createSheet(wb = myWorkbook, sheetName = "AtA0")

            # Add data to workbook
            addDataFrame(x = OA[["summary.auc"]],        sheet = sheet_AUC,    startRow = 1, row.names = FALSE)
            addDataFrame(x = OA[["summary.slope"]],      sheet = sheet_slopes, startRow = 1, row.names = FALSE)
            addDataFrame(x = OA[["summary.areachange"]], sheet = sheet_AtA0,   startRow = 1, row.names = FALSE)

            # Save workbok
            xlsxfile_summary <- paste0(OA[["outputfolder"]], "/FIS_summary_", OA[["slider_tf"]], "min.xlsx")
            message(paste0("        Saving summary data to ", xlsxfile_summary))
            saveWorkbook(myWorkbook, xlsxfile_summary)
            message("    Saved summarized data!")
            message("")



            ## 5.4. Plots ------------------------------------

            message("    Starting export of plot images")

            path_plot_plate      <- paste0(OA[["outputfolder"]], "/plot_overview.png")
            path_plot_AUC        <- paste0(OA[["outputfolder"]], "/plot_AUC_", isolate(OA[["slider_tf"]]), "min.png")
            path_plot_slopes     <- paste0(OA[["outputfolder"]], "/plot_initialswellingrate_", isolate(OA[["slider_tf"]]), "min.png")
            path_plot_normalized <- paste0(OA[["outputfolder"]], "/plot_AtA0_", isolate(OA[["slider_tf"]]), "min.png")
            path_plot_titration  <- paste0(OA[["outputfolder"]], "/plot_titration_AUC_", isolate(OA[["slider_tf"]]), "min.png")

            png(path_plot_plate, width = 1920, height = 1080)
            print(g_plate)
            dev.off()
            message(paste0("        Saved plate overview plot to: ", path_plot_plate))

            png(path_plot_titration, width = 1920, height = 1080)
            print(g_tit)
            dev.off()
            message(paste0("        Saved titration plot (AUC) to: ", path_plot_titration))

            png(path_plot_AUC, width = 1920, height = 1080)
            print(g_AUC)
            dev.off()
            message(paste0("        Saved AUC plot to: ", path_plot_AUC))

            png(path_plot_slopes, width = 1920, height = 1080)
            print(g_slopes)
            dev.off()
            message(paste0("        Saved initial swelling rate plot to: ", path_plot_slopes))

            png(path_plot_normalized, width = 1920, height = 1080)
            print(g_norm)
            dev.off()
            message(paste0("        Saved AtA0 plot to: ", path_plot_normalized))

            message("    Saved all plots!")
            message("")



            ## 5.5. Analysis settings ------------------------------------

            message("    Starting export of analysis settings")

            # Compute additional settings
            output_datetime <- format(Sys.time(), "%d-%m-%y_%H-%M")
            output_date     <- format(Sys.time(), "%d %B %Y")
            output_time     <- format(Sys.time(), "%H:%M")
            n_wells         <- length(unique(OA[["dataset.raw"]][[OA[["Settings_colWell"]]]]))
            n_comp          <- length(unique(OA[["dataset.raw"]][[OA[["Settings_colCompound"]]]]))
            n_treat         <- length(unique(interaction(
                                    OA[["dataset.raw"]][[OA[["Settings_colCompound"]]]],
                                    OA[["dataset.raw"]][[OA[["Settings_colConcentration"]]]]
                               )))
            df_QC           <- OA[["dataset.norm.QC"]][!is.na(OA[["dataset.norm.QC"]]$sumarea), c("well", "compound", "concentration")]
            n_wells_QC      <- length(unique(df_QC$well))
            n_comp_QC       <- length(unique(df_QC$compound))
            n_treat_QC      <- length(unique(interaction(df_QC$compound, df_QC$concentration)))

            if(!dir.exists(OA[["outputfolder"]])) dir.create(OA[["outputfolder"]], recursive = TRUE)
            LogFileName <- paste0(OA[["outputfolder"]], "/FISanalysis_", output_datetime ,".log")
            LogFile     <- file(LogFileName, "w")
            message(paste0("        Saving analysis settings to ", LogFileName))

            cat("╔═════════════════════════════════════════════════════════════════════════════╗", file = LogFile, sep="\n", append=TRUE)
            cat("║  Organoid Analyst                                                           ║", file = LogFile, sep="\n", append=TRUE)
            cat(paste0(c("║  ", OAversion, rep(" ", 75 - nchar(OAversion)), "║"), collapse = ""), file = LogFile, sep="\n", append=TRUE)
            cat(paste0(c("║  ", output_date, rep(" ", 75 - nchar(output_date)), "║"), collapse = ""), file = LogFile, sep="\n", append=TRUE)
            cat(paste0(c("║  ", output_time, rep(" ", 75 - nchar(output_time)), "║"), collapse = ""), file = LogFile, sep="\n", append=TRUE)
            cat("║                                                                             ║", file = LogFile, sep="\n", append=TRUE)
            cat("║  Analysis Settings                                                          ║", file = LogFile, sep="\n", append=TRUE)
            cat("╚═════════════════════════════════════════════════════════════════════════════╝", file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Raw data folder: ", OA[["sourcefolder"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Results folder:  ", OA[["outputfolder"]]), file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("┌-----------------------------------------------------------------------------┐", file = LogFile, sep="\n", append=TRUE)
            cat("|  SAVED FILES                                                                |", file = LogFile, sep="\n", append=TRUE)
            cat("└-----------------------------------------------------------------------------┘", file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Table (raw data)                                  │  ", xlsxfile_raw), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Table (normalized data)                           │  ", xlsxfile_norm), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Table (summarized data)                           │  ", xlsxfile_summary), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Plot (plate overview)                             │  ", path_plot_plate), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Plot (titration: AUC)                             │  ", path_plot_titration), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Plot (initial swelling rate)                      │  ", path_plot_slopes), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Plot (AUC)                                        │  ", path_plot_AUC), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Plot (At/A0)                                      │  ", path_plot_normalized), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Analysis settings (this file)                     │  ", LogFileName), file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("┌-----------------------------------------------------------------------------┐", file = LogFile, sep="\n", append=TRUE)
            cat("|  EXPERIMENT SETTINGS                                                        |", file = LogFile, sep="\n", append=TRUE)
            cat("└-----------------------------------------------------------------------------┘", file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Time resolution (minutes per timepoint)           │  ", OA[["Settings_TimeRes"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Name of the column with AREA values               │  ", OA[["Settings_colArea"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Name of the column with TIME values               │  ", OA[["Settings_colTime"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Name of the column with WELL values               │  ", OA[["Settings_colWell"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Name of the column with COMPOUND names            │  ", OA[["Settings_colCompound"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Name of the column with CONCENTRATION values      │  ", OA[["Settings_colConcentration"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Number of rows                                    │  ", OA[["Settings_numRows"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Number of columns                                 │  ", OA[["Settings_numCols"]]), file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("┌-----------------------------------------------------------------------------┐", file = LogFile, sep="\n", append=TRUE)
            cat("|  QUALITY CONTROL SETTINGS                                                   |", file = LogFile, sep="\n", append=TRUE)
            cat("└-----------------------------------------------------------------------------┘", file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Wells excluded from analysis                      │  ", paste(sort(OA[["QClist"]]), collapse = ", ")), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Name of the column with organoid ID               │  ", OA[["Settings_colOrganoidID"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  ID of invalid organoids                           │  ", OA[["Settings_nameInvalid"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Name of the column with organoid center (X)       │  ", OA[["Settings_colX"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Name of the column with organoid center (Y)       │  ", OA[["Settings_colY"]]), file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("┌-----------------------------------------------------------------------------┐", file = LogFile, sep="\n", append=TRUE)
            cat("|  SEGMENTATION MASK SETTINGS                                                 |", file = LogFile, sep="\n", append=TRUE)
            cat("┌-----------------------------------------------------------------------------┐", file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Generate segmentation masks?                      │  ", OA[["Settings_generateMasks"]]), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Example raw image file name                       │  ", sample_rawImagePath()), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Example raw image file name (after remapping)     │  ", sample_remappedImagePath()), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Correct remapping of raw image files?             │  ", exists_remappedImagePath()), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Suffix of raw image files                         │  ", sample_rawImageSuffix()), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Example CellProfiler/IJ mask file name            │  ", sample_remappedMaskPath()), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Correct remapping of CellProfiler/IJ mask files?  │  ", exists_remappedMaskPath()), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Example Organoid Analyst mask file name           │  ", sample_newMaskPath()), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Example Organoid Analyst labels file name         │  ", sample_newLabelsPath()), file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("┌-----------------------------------------------------------------------------┐", file = LogFile, sep="\n", append=TRUE)
            cat("|  PLOTTING SETTINGS                                                          |", file = LogFile, sep="\n", append=TRUE)
            cat("└-----------------------------------------------------------------------------┘", file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Initial time points                               │  ", paste0(OA[["slider_initial"]][1], " ~ ", OA[["slider_initial"]][2])), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Final experiment time                             │  ", OA[["slider_tf"]]), file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("", file = LogFile, sep="\n", append=TRUE)
            cat("┌-----------------------------------------------------------------------------┐", file = LogFile, sep="\n", append=TRUE)
            cat("|  ANALYZED WELLS                                                             |", file = LogFile, sep="\n", append=TRUE)
            cat("└-----------------------------------------------------------------------------┘", file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Number of used wells (before QC)                  │  ", n_wells), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Number of compounds (before QC)                   │  ", n_comp), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Number of treatments (before QC)                  │  ", n_treat), file = LogFile, sep="\n", append=TRUE)
            cat("  --------------------------------------------------┼-------------------------", file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Number of used wells (after QC)                   │  ", n_wells_QC), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Number of compounds (after QC)                    │  ", n_comp_QC), file = LogFile, sep="\n", append=TRUE)
            cat(paste0("  Number of treatments (after QC)                   │  ", n_treat_QC), file = LogFile, sep="\n", append=TRUE)
            close(LogFile)
            message("    Saved all settings!")
            message("")


            message("Finished exporting data!")


        },
            message = function(m) html("echo_export", m$message, add = TRUE)
        )

    })


    # 6. Help buttons ------------------------------------

    ### 1. LOAD DATA ###
    ### Name of the file with organoid measurements ###
    observeEvent(input$help_Settings_objFileName, {
        showModal(modalDialog(title = "Name of the file with organoid measurements",
                              "Type the name of the file generated by CellProfiler or ImageJ which contains organoid-level measurements. Typically, this is a *.csv file.",
                              easyClose = TRUE))
    })

    ### Choose a '--cellprofiler' folder... ###
    observeEvent(input$help_Settings_folderInput, {
        showModal(modalDialog(title = "Choose a '--cellprofiler' folder...",
                              "Select the folder containing the files generated by CellProfiler or ImageJ. The folder may contain text files and images.",
                              easyClose = TRUE))
    })

    ### 2. SETTINGS ###
    ### Change output folder ###
    observeEvent(input$help_Settings_folderOutput, {
        showModal(modalDialog(title = "Change output folder",
                              "This is the folder where Organoid Analyst will save most files with analysis results.",
                              easyClose = TRUE))
    })

    ### Time resolution (minutes per timepoint) ###
    observeEvent(input$help_Settings_TimeRes, {
        showModal(modalDialog(title = "Time resolution (minutes per timepoint)",
                              "The time difference between two consecutive timelapse frames.",
                              easyClose = TRUE))
    })

    ### Name of the column with AREA values ###
    observeEvent(input$help_Settings_colArea, {
        showModal(modalDialog(title = "Name of the column with AREA values",
                              "The name of the CellProfiler or ImageJ table column which contains organoid cross-sectional area. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### Name of the column with TIME values ###
    observeEvent(input$help_Settings_colTime, {
        showModal(modalDialog(title = "Name of the column with TIME values",
                              "The name of the CellProfiler or ImageJ table column which contains the time information for the timelapse. This is typically the sequential numbering of frames. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### Name of the column with WELL values ###
    observeEvent(input$help_Settings_colWell, {
        showModal(modalDialog(title = "Name of the column with WELL values",
                              "The name of the CellProfiler or ImageJ table column which contains the well number. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### Name of the column with COMPOUND names ###
    observeEvent(input$help_Settings_colCompound, {
        showModal(modalDialog(title = "Name of the column with COMPOUND names",
                              "The name of the CellProfiler or ImageJ table column which contains the name of the compound being tested. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### Name of the column with CONCENTRATION values ###
    observeEvent(input$help_Settings_colConcentration, {
        showModal(modalDialog(title = "Name of the column with CONCENTRATION values",
                              "The name of the CellProfiler or ImageJ table column which contains the compound concentration. Even though it is not mandatory, it is strongly recommended that this column strictly contains numeric values, as this allows Organoid Analyst to compute dose-response curves. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### Number of rows ###
    observeEvent(input$help_Settings_numRows, {
        showModal(modalDialog(title = "Number of rows",
                              "Number of rows in the multiwell plate which generated the results being analyzed. Please specify the actual number of rows in the plate, even if not all wells were imaged or analyzed.",
                              easyClose = TRUE))
    })

    ### Number of columns ###
    observeEvent(input$help_Settings_numCols, {
        showModal(modalDialog(title = "Number of columns",
                              "Number of columns in the multiwell plate which generated the results being analyzed. Please specify the actual number of columns in the plate, even if not all wells were imaged or analyzed.",
                              easyClose = TRUE))
    })

    ### Name of the column with organoid ID ###
    observeEvent(input$help_Settings_colOrganoidID, {
        showModal(modalDialog(title = "Name of the column with organoid ID",
                              "The name of the CellProfiler or ImageJ table column which contains the unique identification for each organoid. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### ID of invalid organoids ###
    observeEvent(input$help_Settings_nameInvalid, {
        showModal(modalDialog(title = "ID of invalid organoids",
                              "Organoids with this ID will be ignored during analysis. The best use of this feature consists in having CellProfiler flag ('NaN') aberrant organoids (e.g. organoids which are not tracked during the entire time lapse) and then exclude those during the Organoid Analyst session. By selecting the 'Allow all organoids' option, this feature is disabled and all organoids are considered.",
                              easyClose = TRUE))
    })

    ### Name of the column with organoid center (X) ###
    observeEvent(input$help_Settings_colX, {
        showModal(modalDialog(title = "Name of the column with organoid center (X)",
                              "The name of the CellProfiler or ImageJ table column which contains the X coordinate for the geometric center of each organoid in the image. This is used by Organoid Analyst to generate segmentation masks. If segmentation masks are not required, this field is ignored. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### Name of the column with organoid center (Y) ###
    observeEvent(input$help_Settings_colY, {
        showModal(modalDialog(title = "Name of the column with organoid center (Y)",
                              "The name of the CellProfiler or ImageJ table column which contains the Y coordinate for the geometric center of each organoid in the image. This is used by Organoid Analyst to generate segmentation masks. If segmentation masks are not required, this field is ignored. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### Column with file path ###
    observeEvent(input$help_Settings_colRawImgPath, {
        showModal(modalDialog(title = "Column with file path",
                              "The name of the CellProfiler or ImageJ table column which contains the location of the raw image file. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### Image root folder name in table ###
    observeEvent(input$help_Settings_pathInTable, {
        showModal(modalDialog(title = "Image root folder name in table",
                              "The parent folder for image files, as it shows in the CellProfiler or ImageJ table. Organoid Analyst uses this information to open images (through Fiji) even if they heve been moved from their original location. If image viewing is not required, this field can be ignored. If both 'folder name in table' and 'folder name in this computer' locations are correctly filled in, Organoid Analyst should be able to find the files and display a 'Found file!' message below. Otherwise, 'File not found.' is displayed.",
                              easyClose = TRUE))
    })

    ### Image root folder name in this computer ###
    observeEvent(input$help_Settings_pathInComputer, {
        showModal(modalDialog(title = "Image root folder name in this computer",
                              "The parent folder for image files, as currently known to this computer's operating system. Organoid Analyst uses this information to open images (through Fiji) even if they heve been moved from their original location. If image viewing is not required, this field can be ignored. If both 'folder name in table' and 'folder name in this computer' locations are correctly filled in, Organoid Analyst should be able to find the files and display a 'Found file!' message below. Otherwise, 'File not found.' is displayed.",
                              easyClose = TRUE))
    })

    ### Generate segmentation masks? ###
    observeEvent(input$help_Settings_generateMasks, {
        showModal(modalDialog(title = "Generate segmentation masks?",
                              "Should Organoid Analyst generate segmentation masks? This feature takes object mask images generated by CellProfiler or ImageJ and the ID of invalid organoids to generate new images where invalid organoids are removed (only outlienes are retained). The Organoid Analyst mask images are a powerful exploratory data analysis tool, as they can be overlaid to the raw images to judge segmentation quality and the impact of object-level quality control. This feature requires that all options in the 'File Remapping Settings' and 'Segmentation Masks Settings' be correctly filled in.",
                              easyClose = TRUE))
    })

    ### Column with masks file path ###
    observeEvent(input$help_Settings_colMaskImgPath, {
        showModal(modalDialog(title = "Column with mask file path",
                              "The name of the CellProfiler or ImageJ table column which contains the location of the segmentation masks. Organoid Analyst automatically lists all column headers. If needed, the data table can be viewed under 'View data > Raw data table (concatenated)'.",
                              easyClose = TRUE))
    })

    ### Masks root folder name in table ###
    observeEvent(input$help_Settings_MimagepathInTable, {
        showModal(modalDialog(title = "Segmentation masks root folder name in table",
                              "The parent folder for segmentation mask images, as it shows in the CellProfiler or ImageJ table. Organoid Analyst uses this information to open images (through Fiji) even if they heve been moved from their original location. If image viewing is not required, this field can be ignored. If both 'folder name in table' and 'folder name in this computer' locations are correctly filled in, Organoid Analyst should be able to find the files and display a 'Found file!' message below. Otherwise, 'File not found.' is displayed.",
                              easyClose = TRUE))
    })

    ### Masks root folder name in this computer ###
    observeEvent(input$help_Settings_MimagepathInComputer, {
        showModal(modalDialog(title = "Segmentation masks root folder name in this computer",
                              "The parent folder for segmentation mask images, as currently known to this computer's operating system. Organoid Analyst uses this information to open images (through Fiji) even if they heve been moved from their original location. If image viewing is not required, this field can be ignored. If both 'folder name in table' and 'folder name in this computer' locations are correctly filled in, Organoid Analyst should be able to find the files and display a 'Found file!' message below. Otherwise, 'File not found.' is displayed.",
                              easyClose = TRUE))
    })

    ### Length of image file suffix ###
    observeEvent(input$help_Settings_rawSuffixLength, {
        showModal(modalDialog(title = "File suffix length",
                              "The length of the suffix on the image file. In the mask file this suffix should be different and specific to the mask file. Organoid Analyst uses this information to open images (through Fiji). If image viewing is not required, this field can be ignored. If both 'folder name in table' and 'folder name in this computer' locations are correctly filled in, Organoid Analyst should be able to find the files and display a 'Found file!' message below. Otherwise, 'File not found.' is displayed.",
                              easyClose = TRUE))
    })

    ### Suffix of CellProfiler masks file ###
    observeEvent(input$help_Settings_maskFileSuffix, {
        showModal(modalDialog(title = "Suffix of CellProfiler masks files",
                              "The suffix of the masks images created by CellProfiler or ImageJ. See below how this information is interpreted. Organoid Analyst uses this information to generate updated masks images, which reflect the QC parameters.",
                              easyClose = TRUE))
    })

    ### Suffix of Organoid Analyst masks ###
    observeEvent(input$help_Settings_OAMasksFileSuffix, {
        showModal(modalDialog(title = "Suffix for Organoid Analyst masks files",
                              "This suffix will identify the organoid masks images generated by Organoid Analyst. See the sample file path to see how this applies to your data.",
                              easyClose = TRUE))
    })

	### Suffix of Organoid Analyst labels ###
	observeEvent(input$help_Settings_OALabelsFileSuffix, {
        showModal(modalDialog(title = "Suffix for Organoid Analyst labels files",
                              "This suffix will identify the organoid labels images generated by Organoid Analyst. See the sample file path to see how this applies to your data.",
                              easyClose = TRUE))
    })

    ### Path to Fiji (only necessary for Windows) ###
    observeEvent(input$help_fiji_binary, {
        showModal(modalDialog(title = "Path to Fiji (only necessary for Windows)",
                              "Location of the Fiji executable file in this computer. Organoid Analyst displays whether it can find Fiji or not.",
                              easyClose = TRUE))
    })

    ### Button Normalize Data ###
    observeEvent(input$help_ButtonNormalize, {
        showModal(modalDialog(title = "Normalize data",
                              "Initiate data normalization. Data normalization comprises: (1) Excluding 'invalid' organoids; (2) Sum the organoid area in each image; (3) Calculate the fold area change and AUC as a function of time; (4) Save all statistics in a file called 'FIS_normalized.xlsx' located in the output folder; (5) Generate segmentation masks images, if required. Once all these steps finish, Organoid Analyst automatically select the plotting tab, where data visualzation and final data export can be performed.",
                              easyClose = TRUE))
    })

    ### 3. PLOTTING ###
    ### Initial time points ###
    observeEvent(input$help_Plotting_initialTimePoints, {
        showModal(modalDialog(title = "Select initial data points",
                              "The time points (in time units) to be used for fitting a linear model reporting the initial swelling rate. Organoid Analyst can show the fitting for each well in the plot below. The kinetic trace in each well can also be colored according to its initial swelling rate.",
                              easyClose = TRUE))
    })

    ### Experiment end timepoint ###
    observeEvent(input$help_Plotting_finalExperimentTime, {
        showModal(modalDialog(title = "Select final experiment time",
                              "The time point (in time units) to serve as the experiment end time. When clicking 'Export data', Organoid Analyst reports the normalized area change and AUC between time zero and the final experiment time.",
                              easyClose = TRUE))
    })

    ### QC: Excluded wells ###
    observeEvent(input$help_QClist, {
        showModal(modalDialog(title = "Select wells you wish to exclude from calculations",
                              "The wells selected in this field will be excluded from analysis. This is useful to exlude wells exhibiting aberrant behavior. All plots below are updated on the fly to reflect the well QC.",
                              easyClose = TRUE))
    })

    ### Wells to open in Fiji ###
    observeEvent(input$help_fijiWells, {
        showModal(modalDialog(title = "Select wells to be opened in Fiji",
                              "Open the time lapses in the specified wells in Fiji. Each well is opened as a separate time lapse. If segmentation masks were generated, they are overlaid onto the raw image. Fiji allows full control over image visualization and analysis.",
                              easyClose = TRUE))
    })

    ### Export data button ###
    observeEvent(input$help_exportFinal, {
        showModal(modalDialog(title = "Export data",
                              "Compute initial swelling speed and other statistics at the final experiment time point. Numerical data is saved in the output folder in a file called 'FIS_summary_xxmin.xlsx'. All plots are also saved, in the same state as they are viewed in this page.",
                              easyClose = TRUE))
    })
})
