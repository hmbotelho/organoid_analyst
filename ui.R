# Load dependencies
source("./functions.r")
setupPackages(dependencies)



shinyUI(navbarPage("Organoid Analyst", id = "mainNavbarPage",

    tabPanel("1. Load data", value = "tabLoad",

        shinyjs::useShinyjs(),

        # Name of the file with organoid measurements
        {fluidRow(
            column(11,
                textInput("Settings_objFileName", "Name of the file with organoid measurements", value = "objects.csv", width = "100%")
            ),
            column(1,
                actionButton("help_Settings_objFileName", "", icon = icon("question-circle"))
            ),
            tags$style(type='text/css', "#help_Settings_objFileName {margin-top: 25px;}")
        )},

        # Choose a '--cellprofiler' or '--ij' folder...
        {fluidRow(
            column(11,
                shinyDirButton("Settings_folderInput", "Choose a '--cellprofiler' or '--ij' folder...", "Select a '--cellprofiler' or '--ij' folder", icon = icon("folder-open"))
            ),
            column(1,
                actionButton("help_Settings_folderInput", "", icon = icon("question-circle"))
            ),
            br(),br(),br()
        )},

        # Multiprocessor options
        {fluidRow(
            column(11,
                   checkboxInput("Settings_parallelize", "Enable multiprocessor support? (if available)", TRUE)
            ),
            column(1,
                   actionButton("help_Settings_parallelize", "", icon = icon("question-circle"))
            ),
            tags$style(type='text/css', "#help_Settings_objFileName {margin-top: 25px;}")
        )},
        
        uiOutput("UI_sourcefolder"),

        # Echo: file concatenation
        verbatimTextOutput("echo_concat")
    ),



    tabPanel("2. Settings", value = "tabSettings",

        wellPanel(
            uiOutput("UI_outputFolder")
        ),
        br(),
        wellPanel(
            uiOutput("UI_ExpSettings")
        ),
        br(),
        wellPanel(
            uiOutput("UI_QCSettings1"),
            uiOutput("UI_QCSettings2")
        ),
        br(),
        wellPanel(
            uiOutput("UI_RemapSettings1"),
            uiOutput("UI_RemapSettings2"),
            uiOutput("UI_RemapSettings3")
        ),
        br(),
        wellPanel(
            uiOutput("UI_MasksSettings1"),
            uiOutput("UI_MasksSettings2"),
            uiOutput("UI_MasksSettings3"),
            uiOutput("UI_MasksSettings4"),
            uiOutput("UI_MasksSettings5"),
            uiOutput("UI_MasksSettings6"),
            uiOutput("UI_MasksSettings7")
        ),
        br(),
        wellPanel(
            uiOutput("UI_ViewSettings1"),
            uiOutput("UI_ViewSettings2")
        ),
        br(),
        uiOutput("UI_ButtonNormalize"),
        hr(),

        fluidRow(
            column(width = 12,
                verbatimTextOutput("echo_norm")
            )
        )
    ),



    tabPanel("3. Plotting", value = "tabPlotting",

        fluidRow(
            column(width = 4,
                wellPanel(
                    h3("Analysis settings"),
                    uiOutput("UI_plotting")
                )
            ),
            column(width = 4,
                wellPanel(
                    h3("Quality control"),
                    uiOutput("UI_QClist")
                )
            ),
            column(width = 4,
                wellPanel(
                   h3("Timelapse viewer"),
                   uiOutput("UI_openFiji"),
                   verbatimTextOutput("selection")
                )
            )
        ),

        hr(),
        fluidRow(
            column(width = 12,
                uiOutput("UI_ExportFinal"),
                verbatimTextOutput("echo_export")
            )
        ),

        hr(),
        fluidRow(
            column(width = 12,
                uiOutput("plotPlate"),
                uiOutput("UI_options_plotPlate"),
                br(),
                hr(),
                plotOutput("plot_titration", height = 500),
                uiOutput("UI_options_plotTitration"),
                br(),
                hr(),
                plotOutput("plot_AUC", height = 500),
                uiOutput("UI_options_plotAUC"),
                br(),
                hr(),
                plotOutput("plot_slopes", height = 500),
                uiOutput("UI_options_plotSlopes"),
                br(),
                hr(),
                plotOutput("plot_normalized", height = 500),
                uiOutput("UI_options_plotNormalized")
            )
        )

    ),


    navbarMenu("View data",
        tabPanel("Raw data table (concatenated)",
            h3("Data Table: Raw data"),
            br(),
            dataTableOutput("table_rawdata")
        ),

        tabPanel("Normalized data",
            h3("Data Table: Normalized data"),
            br(),
            dataTableOutput("table_normalized")
        )
    ),


    tabPanel("About", value = "tabAbout",
        wellPanel(
            p("By Hugo Botelho, April 2020"),
            a(href="mailto:hmbotelho@fc.ul.pt", "hmbotelho@fc.ul.pt"), br(),
            a(href="mailto:hugobotelho@gmail.com", "hugobotelho@gmail.com"), br(),
			a(href="http://webpages.fc.ul.pt/~hmbotelho/", "webpages.fc.ul.pt/~hmbotelho/"), br(), br(),
			h3("Online repository & source code"),
			icon("github"), a(href="https://github.com/hmbotelho/organoid_analyst", "github.com/hmbotelho/organoid_analyst"),
			p("Licensed under the GNU General Public License v3.0"), br(),
			h3("Citing Organoid Analyst"),
			p(HTML("Hagemeijer MC, Vonk AM, Awatade NT, Silva IAL, Tischer C, Hilsenstein V, Beekman JM, Amaral MD, Botelho HM (2020) <strong>An open-source high-content analysis workflow for CFTR function measurements using the forskolin-induced swelling assay</strong> <i>submitted</i>"))
        )
    )
))
