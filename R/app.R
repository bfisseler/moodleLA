
#' moodleLA app
#'
#' Returns a Shiny app to start using shiny::runApp. See examples.
#'
#'
#' @import shiny
#' @import bslib
#' @import dplyr
#' @importFrom arrow write_feather write_parquet
#' @importFrom utils write.csv read.csv
#' @importFrom haven read_sav write_sav
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom pool poolClose poolReturn
#' @importFrom yaml read_yaml write_yaml
#' @importFrom stats setNames
#' @importFrom shinyjs useShinyjs disable enable runjs reset
#' @importFrom tm removeWords
#' @importFrom utils head
#' @examples 
#' # The following line starts the app inside the systems standard browser on port 3000.
#' \dontrun{
#'   shiny::runApp(moodleLA(), port = 3000, launch.browser = TRUE)
#' }
#' @export

moodleLA <- function(){
# --------#
# Globals #
# --------#

dbpMdl <- NA

# --------#
#    UI   #
# --------#

ui <- bslib::page_navbar(
  shinyjs::useShinyjs(),
  lang = "en",
  title = "moodleLA",
  id = "nav",
  header = tags$div(
    conditionalPanel(
      condition = "input.nav !== 'Intro' & input.nav !== 'Pseudonymise Data'",
      bslib::layout_columns(
        col_widths = c(10, 2),
        shiny::selectInput("selectLogdataCourse", "Select course", choices = "", multiple = FALSE),
        shiny::numericInput("selectLogdataCourseID", "Course-ID", value = 1, min = 1),
        fillable = FALSE, fill = FALSE
      )
    ),
    conditionalPanel(
      "false", # always hide the download button
      downloadButton("downloadData"),
      downloadButton("downloadLogdata"),
      downloadButton("downloadMatchdata"),
      downloadButton("downloadMFD"),
      downloadButton("downloadPseuddata")
    )
  ),
  theme = bslib::bs_theme(5, "flatly"),
  navbar_options = bslib::navbar_options(theme = "auto", collapsible = TRUE),
  #begin nav_panel 1
  bslib::nav_panel(title = "Intro",
            shiny::includeMarkdown("frontpage.md")
  ),
  # end nav_panel 1
  
  # ----
  # begin nav_panel 2 - Course Userlist
  # ----
  bslib::nav_panel(title = "Course Userlist", 
            p("This function can be used to retrieve pseudonymised list of all users with the learner role in a Moodle course."),
            #createCourseSelectionUI("pnlUserlist"),
            
            #button
            bslib::input_task_button("btnGetUserlist", "Get user list")
  
  ),
  #end nav_panel 2
  
  # ----
  # begin nav_panel 3 - Moodle Logdata
  # ----
  bslib::nav_panel("Moodle Logdata", 
            p("Use this function to download the log data from all users with the learner role in a Moodle course. You can also preprocess the data using a wrangling algorithm from the list or provide your own R code for preprocessing."),
            p("1. Select a course from the list, or enter the course ID directly."),
            #createCourseSelectionUI("pnlLogdata"),
            p("2. Specify a date range for which the log data should be downloaded."),
            p("The log data will be downloaded from 12:00 AM (00:00) of the first date to 11:59 PM (23:59) of the second date."),
            shiny::dateRangeInput(inputId = "dateRangeLogdata", label = "Date Range", weekstart = 1, width = "100%"),
            p("3. Select data wranglings from the list or enter your own R code snippet"),
            bslib::layout_columns(
              col_widths = c(4, 8),
              shiny::selectInput("selectLogdataWrangling","Select data wrangling", choices = c("As is" = "raw", "R Code" = "R"), selected = "", multiple = FALSE),
              shiny::textAreaInput("textInputLogdataWrangling", "Enter R code for data processing", rows = 10),
              fillable = FALSE, fill = FALSE
            ),
            p("4. Select the preferred output file format."),
            bslib::layout_columns(
              col_widths = c(4, 8),
              shiny::selectInput("selectLogdataOutputFormat","File format:", choices = c("CSV", "Feather", "Parquet"), selected = "CSV", multiple = FALSE),
              fillable = FALSE, fill = FALSE
            ),
            # button
            bslib::input_task_button("btnGetLogdata", "Process Moodle Logdata")
  ),
  # end nav_panel 3
  
  # ----
  # begin nav_panel 4 - Moodle Forum Data
  # ----
  bslib::nav_panel("Moodle Forum Data", 
            p("Use this function to download and pseudonymise all forum messages from a Moodle course."),
            #createCourseSelectionUI("pnlForumdata"),
            shiny::selectInput("selectForumsMFD", "1. Select forums to download.", choices = "", multiple = TRUE, selectize = TRUE, width = "100%"),
            p("2. Select the information you want to remove from the message text."),
            bslib::layout_columns(
              column(6,
                shiny::checkboxInput("chkboxFilterEmailMFD", "Emails", value = TRUE),
                shiny::checkboxInput("chkboxURLMFD", "URLs", value = TRUE),
                shiny::checkboxInput("chkboxPhoneMFD", "Phonenumbers", value = TRUE)
              ),
              column(6,
                shiny::checkboxInput("chkboxNamesMFD", "Names (userlist)", value = TRUE),
                shiny::checkboxInput("chkboxPOSMFD", "Presidio PII removal", value = TRUE),
                shiny::checkboxInput("chkboxReplaceMFD", "Replace with placeholder", value = TRUE)
              ),
              fillable = FALSE, fill = FALSE
            ),
            p("Microsoft Presidio, running in Docker locally or remote, can be used to remove personal data. You must specify the host address and port for both analyzer and anonymizer."),
            bslib::layout_columns(
              shiny::textInput("presidioAnalyzer", "Presidio Analyzer", value = "http://localhost:5002/analyze", placeholder = "http://localhost:5002/analyze"),
              shiny::textInput("presidioAnonymizer", "Presidio Anonymizer", value = "http://localhost:5001/anonymize", placeholder = "http://localhost:5001/anonymize"),
              shiny::selectInput("presidioLang", "Language", choices = listLangCodes, selected = "en"),
              fillable = FALSE, fill = FALSE, col_widths = c(5, 5, 2)
            ),
            p("3. Select the preferred output file format."),
            bslib::layout_columns(
              col_widths = c(4, 8),
              shiny::selectInput("selectOutputFormatMFD","File format:", choices = c("CSV", "Feather", "Parquet"), selected = "CSV", multiple = FALSE),
              fillable = FALSE, fill = FALSE
            ),
            # button
            bslib::input_task_button("btnGetMFD", "Get Moodle forum messages")
  ),
  # end nav_panel 4
  
  # ----
  # begin nav_panel 5 - Match & Pseudonymise Data
  # ----
  bslib::nav_panel("Match & Pseudonymise Data",
            p("Use this function to load existing data such as exam results or survey data and match the data with the users in a Moodle course."),
            p("1. Load the data file. If you are loading a CSV file, please also select the delimiter that is used to separate fields within a record."),
            bslib::layout_columns(
              shiny::selectInput( 
                "selectSurveyCSVSep", 
                "CSV separator:", 
                list(";", ",") 
              ),
              shiny::fileInput("surveyData", "Load the data file (CSV, XLSX or SAV):", accept = c(".csv", ".sav", ".xlsx")),
              fillable = FALSE, fill = FALSE, col_widths = c(2, 10)
            ),
            p("2. Inspect the data and define missing values."),
            bslib::layout_columns(
              shiny::actionButton("btnInspectSurveyData", "Inspect Data", style = "margin-top:2em"),
              shiny::textInput("txtSurveyMissingValues", label = "(Optional) Enter values that represent missings in the data", placeholder = "-99, -66"),
              fillable = FALSE, fill = FALSE, col_widths = c(2, 10)
            ),
            p("3. Specify the variable in the data set that is used as the personal identifier. This variable will then be matched against the user list and pseudonymised."),
            bslib::layout_columns(
              # select variable to match on
              shiny::selectizeInput("selectSurveyMatchingVar", "Select variable in data file to match on:", choices = "", multiple = FALSE),
              
              # matching variable is...
              shiny::selectInput("selectSurveyMatchingVarIs", "Matching variable is:", choices = c("Moodle-ID", "Email", "Group-ID"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
              fillable = FALSE, fill = FALSE
            ),
            # select variable(s) to remove after matching
            shiny::selectizeInput("selectSurveyRemoveVars", "4. (Optional) Select variable(s) to remove after matching and pseudonymisation:", choices = "", multiple = TRUE, width = "100%"),
            
            # button
            bslib::input_task_button("btnSurveyMatching", "Match & pseudonymise survey data")
            ),
  # end nav_panel 5
  
  # begin nav_panel 6 - pseudonymise data
  # ----
  bslib::nav_panel("Pseudonymise Data",
            p("Use this function to load existing data and select variables for pseudonymisation. Please note: This function won't create new variables but simply replace the content of 
              the selected variables."),
            p("1. Load the data file. If you are loading a CSV file, please also select the delimiter that is used to separate fields within a record."),
            bslib::layout_columns(
              selectInput( 
                "selectPseudDataCSVSep", 
                "CSV separator:", 
                list(";", ",") 
              ),
              fileInput("pseudData", "Load the data file (CSV, XLSX or SAV):", accept = c(".csv", ".sav", ".xlsx")),
              fillable = FALSE, fill = FALSE, col_widths = c(2, 10)
            ),
            p("2. Inspect the data and define missing values."),
            bslib::layout_columns(
              shiny::actionButton("btnInspectPseudData", "Inspect Data", style = "margin-top:2em"),
              shiny::textInput("txtPseudDataMissingValues", label = "(Optional) Enter values that represent missings in the data", placeholder = "-99, -66"),
              fillable = FALSE, fill = FALSE, col_widths = c(2, 10)
            ),
            # select variable(s) to remove after matching
            shiny::selectizeInput("selectPseudDataVars", "3. Select variable(s) for pseudonymisation:", choices = "", multiple = TRUE, width = "100%"),
            
            # select variable(s) to remove after matching
            shiny::selectizeInput("selectPseudDataRemoveVars", "4. (Optional) Select variable(s) to remove after pseudonymisation:", choices = "", multiple = TRUE, width = "100%"),
            # button
            bslib::input_task_button("btnPseudData", "Pseudonymise data")
  ),
  # end nav_panel 6
  
  # -------
  # sidebar
  # -------
  sidebar = bslib::sidebar(
    id = "sidebar",
    width = "30%",
    title = "Project configuration",
    shiny::conditionalPanel(
      "false", # always hide the download button
      # thei fileInput is triggered using shinyjs::click
      shiny::fileInput("loadProjConfig", label = "", buttonLabel = "Load", accept = ".yml")
    ),
    bslib::layout_columns(
      shiny::actionButton("newProjConfig", "New", icon = icon("file", lib = "glyphicon", width = "50%")),
      shiny::actionButton("loadProjConfigProxy", "Load", icon = icon("floppy-open", lib = "glyphicon", width = "50%")),
      shiny::downloadButton("saveProjConfig", "Save", icon = icon("floppy-save", lib = "glyphicon", width = "50%")),
      col_width = c(3,3,3)
    ),
    shiny::actionButton("btnConnectDB", "Connect", icon = icon("transfer", lib = "glyphicon", width = "50%")),
    shiny::textInput("projname", "Project name:"),
    bslib::accordion(open = TRUE, multiple = TRUE,
      bslib::accordion_panel(id = "dbConfig", title = "Database configuration",
                             shiny::textInput("dbname", "Database name:"),
                             shiny::selectInput("dbtype", "Database type:",
                                                list("Postgres")),
                             shiny::textInput("dbhost", "Host:"),
                             shiny::numericInput("dbport", "Port:", value = 5432, min = 1, max = 65535),
                             shiny::textInput("dbprefix", "Table prefix:"),
                             shiny::textInput("dbuser", "User:"),
                             shiny::passwordInput("dbpassword", "Password:") #,
                             #shiny::actionButton("textProjConfig", "Test DB Connection", icon = icon("log-in", lib = "glyphicon", width = "50%"))
                             ),
      bslib::accordion_panel(id = "pepperConfig", title = "Pepper", open = TRUE,
                             shiny::textInput("pepper", "Pepper:", width = "100%"),
                             shiny::actionButton("genPepper", "New pepper", icon = icon("dice-three", lib = "font-awesome", width = "50%"))
                             )
    )
  )
  # end sidebar
) # bslib::page_navbar(

# Define server logic
server <- function(input, output, session) {
  #shinyjs::hide("saveProjConfig")
  
  # onStart: deactivate all btn elements
  # ----
  shinyjs::disable("saveProjConfig")
  shinyjs::disable("btnConnectDB")
  shinyjs::disable("btnGetUserlist")
  shinyjs::disable("btnGetLogdata")
  shinyjs::disable("btnSurveyMatching")
  shinyjs::disable("btnPseudData")
  shinyjs::disable("btnGetMFD")
  shinyjs::disable("btnInspectPseudData")
  shinyjs::disable("btnInspectSurveyData")
  shinyjs::disable("chkboxPOSMFD")
  
  # ----------
  # validators
  # ----------
  
  # input validator for project settings; valid project settings plus valid pepper activate
  iv_db <- shinyvalidate::InputValidator$new()
  iv_db$add_rule("projname", shinyvalidate::sv_required())
  iv_db$add_rule("dbtype", shinyvalidate::sv_required())
  iv_db$add_rule("dbname", shinyvalidate::sv_required())
  iv_db$add_rule("dbhost", shinyvalidate::sv_required())
  #iv_db$add_rule("dbhost", shinyvalidate::sv_regex("^[A-Za-z0-9-_]+\\.[A-Za-z0-9]+\\.[A-Za-z]{2,}|(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$", perl = TRUE, message = "Enter valid URL for host"))
  iv_db$add_rule("dbhost", shinyvalidate::sv_regex("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?", perl = TRUE, message = "Enter valid URL for host"))
  iv_db$add_rule("dbport", shinyvalidate::sv_required())
  iv_db$add_rule("dbprefix", shinyvalidate::sv_required())
  iv_db$add_rule("dbuser", shinyvalidate::sv_required())
  iv_db$add_rule("dbpassword", shinyvalidate::sv_required())
  iv_db$enable()
  
  # input validator for pepper only; valid pepper activates 
  iv_pepper <- shinyvalidate::InputValidator$new()
  iv_pepper$add_rule("pepper", shinyvalidate::sv_required())
  iv_pepper$add_rule("pepper", shinyvalidate::sv_regex("^(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9])(?=.*?[#?!@$ %^&*]).{12,50}$", perl = TRUE, message = "Pepper should be 12 to 20 chars, containing upper and lower case letters, at least one number and one special character (#?!@$ %^&*)."))
  iv_pepper$enable()
  
  # validator for course list
  iv_cl <- shinyvalidate::InputValidator$new()
  iv_cl$add_rule("selectLogdataCourse", shinyvalidate::sv_required())
  iv_cl$enable()
  
  # validator for Moodle Logdata
  iv_ml <- shinyvalidate::InputValidator$new()
  iv_ml$add_rule("selectLogdataCourse", shinyvalidate::sv_required())
  iv_ml$add_rule("dateRangeLogdata", shinyvalidate::sv_required())
  iv_ml$add_rule("selectLogdataWrangling", shinyvalidate::sv_required())
  iv_ml$add_rule("selectLogdataOutputFormat", shinyvalidate::sv_required())
  iv_ml$add_rule("pepper", shinyvalidate::sv_required())
  iv_ml$add_rule("pepper", shinyvalidate::sv_regex("^(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9])(?=.*?[#?!@$ %^&*]).{12,50}$", perl = TRUE, message = "Pepper should be 12 to 20 chars, containing upper and lower case letters, at least one number and one special character (#?!@$ %^&*)."))
  iv_ml$enable()
  
  # validator for Moodle forum data
  iv_mfd <- shinyvalidate::InputValidator$new()
  iv_mfd$add_rule("selectLogdataCourse", shinyvalidate::sv_required())
  iv_mfd$add_rule("selectOutputFormatMFD", shinyvalidate::sv_required())
  iv_mfd$add_rule("selectForumsMFD", shinyvalidate::sv_required())
  iv_mfd$enable()
  
  # validator for Match & Pseudonymise Data
  iv_mpd <- shinyvalidate::InputValidator$new()
  iv_mpd$add_rule("selectLogdataCourse", shinyvalidate::sv_required())
  iv_mpd$add_rule("surveyData", shinyvalidate::sv_required())
  iv_mpd$add_rule("selectSurveyMatchingVar", shinyvalidate::sv_required())
  iv_mpd$add_rule("selectSurveyMatchingVarIs", shinyvalidate::sv_required())
  iv_mpd$add_rule("pepper", shinyvalidate::sv_required())
  iv_mpd$add_rule("pepper", shinyvalidate::sv_regex("^(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9])(?=.*?[#?!@$ %^&*]).{12,50}$", perl = TRUE, message = "Pepper should be 12 to 20 chars, containing upper and lower case letters, at least one number and one special character (#?!@$ %^&*)."))
  iv_mpd$enable()
  
  # validator for pseudonymise data
  iv_pd <- shinyvalidate::InputValidator$new()
  iv_pd$add_rule("pseudData", shinyvalidate::sv_required())
  iv_pd$add_rule("selectPseudDataVars", shinyvalidate::sv_required())
  iv_pd$add_rule("pepper", shinyvalidate::sv_required())
  iv_pd$add_rule("pepper", shinyvalidate::sv_regex("^(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9])(?=.*?[#?!@$ %^&*]).{12,50}$", perl = TRUE, message = "Pepper should be 12 to 20 chars, containing upper and lower case letters, at least one number and one special character (#?!@$ %^&*)."))
  iv_pd$enable()
  
  # --------- #
  # variables #
  # --------- #
  
  # configuration stored as reactiveValues
  config <- reactiveValues()
  observe({
    config$projname = input$projname
    config$dbname = input$dbname
    config$dbtype = input$dbtype
    config$dbhost = input$dbhost
    config$dbport = input$dbport
    config$dbprefix = input$dbprefix
    config$dbuser = input$dbuser
    config$dbpassword = input$dbpassword
    config$pepper = input$pepper
  })
  
  courselist <- reactiveVal(NULL)
  exportData <- reactiveVal(NULL) # this reactive store the processed data for automatic download, see: https://stackoverflow.com/questions/75675984/r-shiny-how-to-have-an-action-button-that-automatically-downloads-a-csv-file
  importData <- NULL # reactiveVal(NULL) # reactive used to store input data uploaded, e.g. for matching survey data
  importDataFilename <- NULL #reactiveVal()
  
  # --------- 
  # reactives 
  # --------- 
  
  # ------------- 
  # configuration 
  # -------------
  
  # show downloadButton only if config is complete
  observeEvent(iv_db$is_valid(), {
    if(iv_db$is_valid()) {
      shinyjs::enable("saveProjConfig")
      shinyjs::enable("btnConnectDB")
    }
    else {
      shinyjs::disable("saveProjConfig")
      shinyjs::disable("btnConnectDB")
    }
  })
  
  # observe actionButton saveProjConfig
  output$saveProjConfig <- downloadHandler(
    #save config
    filename = function() {
      paste0(input$projname, "-config.yml")
    },
    content = function(file) {
      # write reactive value to yaml
      req(config)
      config_list <- list(
        projname = config$projname,
        dbname = config$dbname,
        dbtype = config$dbtype,
        dbhost = config$dbhost,
        dbport = config$dbport,
        dbprefix = config$dbprefix,
        dbuser = config$dbuser,
        dbpassword = config$dbpassword,
        pepper = config$pepper
      )
      yaml::write_yaml(config_list, file)
    }
  )
  
  # observe loadProjConfigProxy
  observeEvent(input$loadProjConfigProxy, {
    shinyjs::runjs("$('#loadProjConfig')[0].click();") # DOWNLOAD BUTTON
  })
  
  
  # upload project configuration
  observeEvent(input$loadProjConfig, {
    req(input$loadProjConfig)
    # read file
    config_data <- yaml::read_yaml(input$loadProjConfig$datapath)
    
    # enter values into input fields
    # project name
    tryCatch({
      if (!is.na(config_data$projname[1])) {
        updateTextInput(session, "projname", value = config_data$projname[1])
      }
    }, error = function(e) {
      showNotification("Error reading configuration file. Please check project name.", type = "warning", duration = 5) #, e$message)
    }) # end tryCatch
    # dbname
    tryCatch({
      if (!is.na(config_data$dbname[1])) {
        updateTextInput(session, "dbname", value = config_data$dbname[1])
      }
    }, error = function(e) {
      showNotification("Error reading configuration file. Please check database name.", type = "warning", duration = 5) #, e$message)
    }) # end tryCatch
    # dbtype
    tryCatch({
      if (!is.na(config_data$dbtype[1])) {
        updateSelectInput(session, "dbtype", selected = config_data$dbtype[1])
      }
    }, error = function(e) {
      showNotification("Error reading configuration file. Please check database type.", type = "warning", duration = 5) #, e$message)
    }) # end tryCatch
    # dbhost
    tryCatch({
      if (!is.na(config_data$dbhost[1])) {
        updateTextInput(session, "dbhost", value = config_data$dbhost[1])
      }
    }, error = function(e) {
      showNotification("Error reading configuration file. Please check database host.", type = "warning", duration = 5) #, e$message)
    }) # end tryCatch
    # dbport
    tryCatch({
      if (!is.na(config_data$dbport[1])) {
        updateTextInput(session, "dbport", value = config_data$dbport[1])
      }
    }, error = function(e) {
      showNotification("Error reading configuration file. Please check database port.", type = "warning", duration = 5) #, e$message)
    }) # end tryCatch
    # dbprefix
    tryCatch({
      if (!is.na(config_data$dbprefix[1])) {
        updateTextInput(session, "dbprefix", value = config_data$dbprefix[1])
      }
    }, error = function(e) {
      showNotification("Error reading configuration file. Please check table prefix.", type = "warning", duration = 5) #, e$message)
    }) # end tryCatch
    # dbuser
    tryCatch({
      if (!is.na(config_data$dbuser[1])) {
        updateTextInput(session, "dbuser", value = config_data$dbuser[1])
      }
    }, error = function(e) {
      showNotification("Error reading configuration file. Please check table prefix.", type = "warning", duration = 5) #, e$message)
    }) # end tryCatch
    # dbpassword
    tryCatch({
      if (!is.na(config_data$dbpassword[1])) {
        updateTextInput(session, "dbpassword", value = config_data$dbpassword[1])
      }
    }, error = function(e) {
      showNotification("Error reading configuration file. Please check table prefix.", type = "warning", duration = 5) #, e$message)
    }) # end tryCatch
    # pepper
    tryCatch({
      if (!is.na(config_data$pepper[1])) {
        updateTextInput(session, "pepper", value = config_data$pepper[1])
      }
    }, error = function(e) {
      showNotification("Error reading configuration file. Please check pepper.", type = "warning", duration = 5) #, e$message)
    }) # end tryCatch
  })
  
  # new project configuration
  observeEvent(input$newProjConfig, {
    updateTextInput(session, "projname", value = "")
    updateTextInput(session, "dbname", value = "")
    updateSelectInput(session, "dbtype", selected = "")
    updateTextInput(session, "dbhost", value = "")
    updateTextInput(session, "dbport", value = "")
    updateTextInput(session, "dbprefix", value = "")
    updateTextInput(session, "dbuser", value = "")
    updateTextInput(session, "dbpassword", value = "")
    updateTextInput(session, "pepper", value = "")
  })
  
  # observe actionButton genPepper
  observeEvent(input$genPepper, {
    if (input$pepper == "") {
      updateTextInput(
        session,
        "pepper",
        value = genPepper()
      )  
    } else {
      showModal(modalDialog(
        title = "Confirmation",
        HTML("<p>This project already has a pepper set. Generating a new pepper will result in different pseudonymizations.</p><p>Are you sure you want to create a new pepper?</p>"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("No"),
          actionButton("yes_btn", "Yes")
        ),
        fade = FALSE
      ))
    }
  })
  
  observeEvent(input$yes_btn, {
    removeModal()
    updateTextInput(
      session,
      "pepper",
      value = genPepper()
    )
  })
  
  # observe btnConnectDB
  observeEvent(input$btnConnectDB, {
    tryCatch({
      dbpMdl <<- mdl_connect("postgresql", config$dbhost, config$dbport, config$dbname, config$dbuser, config$dbpassword)
    }, warning = function(w) {
      showNotification('Could not connect to database.','',type = "error")
      return()
    }, error = function(e) {
      showNotification('Could not connect to database.','',type = "error")
      return()
    })

    if(class(dbpMdl)[1] == "Pool") {
      showNotification("DB connection established.", type = "message")
      bslib::sidebar_toggle(id = "sidebar", open = FALSE)
      # now populate selectList with courses
      tryCatch({
        courselist <- mdl_courses(dbpMdl, config$dbprefix)
        updateSelectInput(session, "selectLogdataCourse", choices = stats::setNames(courselist$courseid, courselist$fullname), label = NULL)
      }, warning = function(w) {
        showNotification('Could not retrieve course list from database.','',type = "error")
        return()
      }, error = function(e) {
        showNotification('Could not retrieve course list from database.','',type = "error")
        return()
      })
    }
    else{
      #showNotification("Could not connect to database.", type = "error", duration = 5)
    }
  }
  )
  
  
  # --------- 
  # Moodle Logdata 
  # --------- 
  
  # observe Validator
  observeEvent(iv_ml$is_valid(), {
    if(iv_ml$is_valid()){
      shinyjs::enable("btnGetLogdata")
    }
    else{
      shinyjs::disable("btnGetLogdata")
    }
  })
  
  # dateRangeLogdata
  observeEvent(input$dateRangeLogdata, {
    if (!is.null(input$dateRangeLogdata[1]) && !is.null(input$dateRangeLogdata[2])) {
      # adjust second date in case the user changes it to range1 > range
      if (input$dateRangeLogdata[1] > input$dateRangeLogdata[2]) {
        # adjust date
        updateDateRangeInput(session, "dateRangeLogdata",
                             start = input$dateRangeLogdata[1],
                             end = input$dateRangeLogdata[1])
      }
    }
  })
  
  # btnGetLogdata
  observeEvent(input$btnGetLogdata,{
    req(input$selectLogdataCourse)
    # add additional req checks here
    
    # build sql for logdata
    shinyjs::runjs("$('#btnGetLogdata').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Querying database...');")
    startTimestamp <- as.integer(as.POSIXct(input$dateRangeLogdata[1]))
    stopTimestamp <- as.integer(as.POSIXct(input$dateRangeLogdata[2])) + 86399
    
    tryCatch({
      course_id <- as.integer(input$selectLogdataCourse)
      logdata <- mdl_logdata(dbpMdl, config$dbprefix, course_id, startTimestamp, stopTimestamp)
      userlist <- mdl_userlist(dbpMdl, config$dbprefix , course_id)
      #courseobjects <- mdl_course_objects(dbpMdl, config$dbprefix, course_id)
    }, warning = function(w) {
      showNotification('Could not retrieve logdata.','',type = "error")
      return()
    }, error = function(e) {
      showNotification('Could not retrieve logdata.','',type = "error")
      return()
    }, finally = {
      shinyjs::runjs("$('#btnGetLogdata').text('Process Moodle Logdata');")
    })
    
    if(nrow(logdata) == 0){
      showNotification('No logdata found.','', type = "warning")
      shinyjs::runjs("$('#btnGetLogdata').text('Process Moodle Logdata');")
      return()
    }
    
    logdata <- logdata |> dplyr::filter(!is.na(username) & username != "") |> dplyr::select(-username, -relatedusername)
    
    # pseudonymze
    shinyjs::runjs("$('#btnGetLogdata').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Pseudonymizing data...');")
    
    userlist$hashuser <- pseudonymize(userlist$username, config$pepper)
    logdata <- left_join(logdata, userlist |> dplyr::select(id, hashuser), by = c("userid" = "id"))
    colnames(userlist)[colnames(userlist) == "hashuser"] <- "relatedhashuser"
    logdata <- left_join(logdata, userlist |> dplyr::select(id, relatedhashuser), by = c("relateduserid" = "id"))
    
    logdata <- logdata |> dplyr::select(id, hashuser, relatedhashuser, courseid:objectid, objectname, crud, edulevel)
    
    # additional data processing
    shinyjs::runjs("$('#btnGetLogdata').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Processing data...');")
    
    # set variable for data export
    shinyjs::runjs("$('#btnGetLogdata').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Preparing download...');")
    exportData(logdata)
    shinyjs::runjs("$('#downloadLogdata')[0].click();") # DOWNLOAD BUTTON
    shinyjs::runjs("$('#btnGetLogdata').text('Process Moodle Logdata');")
  })
  
  # downloadLogdata
  output$downloadLogdata <- downloadHandler(
    filename = function() {
      fn <- genFileName(projname = config$projname, eventname = "logdata", courseid = as.integer(input$selectLogdataCourse))
      if(input$selectLogdataOutputFormat == "CSV"){
        paste0(fn, ".csv")
      } else if(input$selectLogdataOutputFormat == "Parquet"){
        paste0(fn, ".parquet")
      } else if(input$selectLogdataOutputFormat == "Feather"){
        paste0(fn, ".feather")
      }
    },
    content = function(file) {
      if(input$selectLogdataOutputFormat == "CSV"){
        utils::write.csv(exportData(), file, row.names = FALSE, quote = TRUE)
      } else if(input$selectLogdataOutputFormat == "Parquet"){
        arrow::write_parquet(exportData(), file)
      } else if(input$selectLogdataOutputFormat == "Feather"){
        arrow::write_feather(exportData(), file)
      }
    })
  
  # -----------------
  # Moodle Forum Data
  # -----------------
  
  # observe Validator
  observeEvent(iv_mfd$is_valid(), {
    if(iv_mfd$is_valid()){
      shinyjs::enable("btnGetMFD")
    }
    else{
      shinyjs::disable("btnGetMFD")
    }
  })
  
  # observe changes in selectCourse
  observeEvent(input$selectLogdataCourse, {
    req(input$selectLogdataCourse)
    # get all forums from course
    cid <- as.integer(input$selectLogdataCourse)
    tryCatch({
      forums <- mdl_forumlist(dbpMdl, config$dbprefix, cid)
    },error = function(e) {
      showNotification('Could not retrieve forum liar.','',type = "error")
      return()
    })
    # update selectForumsMFD
    updateSelectInput(session, "selectForumsMFD", 
                      choices = setNames(forums$id, forums$name), selected = forums$id, label = NULL)
  })
  
  # btnGetMFD
  observeEvent(input$btnGetMFD,{
    req(input$selectLogdataCourse)
    # add additional req checks here
    
    # build sql for logdata
    shinyjs::runjs("$('#btnGetMFD').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Querying database...');")
    
    cid <- as.integer(input$selectLogdataCourse) # courseid
    fid <- as.integer(input$selectForumsMFD) # forums id
    
    if(length(fid) == 0){
      showNotification('No forum data found.','', type = "warning")
      shinyjs::runjs("$('#btnGetMFD').text('Get Moodle forum messages');")
      return()
    }
    
    tryCatch({
      forumdata <- mdl_forumdata(dbpMdl, config$dbprefix, cid, fid)
    }, error = function(e) {
      showNotification('Could not retrieve forum.','',type = "error")
      return(NULL)
    })
    
    if(nrow(forumdata) == 0){
      showNotification('No forum data found.','', type = "warning")
      shinyjs::runjs("$('#btnGetMFD').text('Get Moodle forum messages');")
      return(NULL)
    }
    
    # convert to text
    shinyjs::runjs("$('#btnGetMFD').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Converting to text...');")
    forumdata$message <- htm2txt(forumdata$message)
    
    # remove user names
    if(input$chkboxNamesMFD){
      shinyjs::runjs("$('#btnGetMFD').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Removing usernames...');")
      tryCatch({
        user <- mdl_userlist(dbpMdl, config$dbprefix, cid)
      }, error = function(e) {
        showNotification('Could not retrieve user list.','',type = "error")
        shinyjs::runjs("$('#btnGetLogdata').text('Get Moodle forum messages');")
        return(NULL)
      })
      
      firstnames <- unlist(strsplit(user$firstname, " +"))
      lastnames <- unlist(strsplit(user$lastname, " +"))
      forumdata$message <- stringr::str_replace_all(forumdata$message, "\"", "")
      #forumdata$message <- sapply(forumdata$message, remove_names, firstnames = firstnames, lastnames = lastnames)
      forumdata$message <- remove_names(forumdata$message, firstnames, lastnames)
    }
    
    # pseudonymising text
    shinyjs::runjs("$('#btnGetMFD').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Pseudonymising text...');")
    forumdata$message <- pseudonymize_messages(forumdata$message, 
                                               remove_email = input$chkboxFilterEmailMFD,
                                               remove_url = input$chkboxURLMFD,
                                               remove_phonenumber = input$chkboxPhoneMFD,
                                               placeholder = input$chkboxReplaceMFD)
    # run Presidio here
    if(input$chkboxPOSMFD){
      shinyjs::runjs("$('#btnGetMFD').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Pseudonymising text using Presidio...');")
      forumdata$message <- presidio_pseudonymize_text(text = forumdata$message, 
                                                      language = input$presidioLang,
                                                      analyzer_url    = input$presidioAnalyzer,
                                                      anonymizer_url  = input$presidioAnonymizer)
    }
    
    # pseudonymze
    shinyjs::runjs("$('#btnGetMFD').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Pseudonymising data...');")
    forumdata$hashuser <- pseudonymize(forumdata$username, config$pepper)
   
    
    # set variable for data export
    forumdata <- forumdata |> dplyr::select(id, hashuser, discussion, parent_id, created:forumname)
    shinyjs::runjs("$('#btnGetMFD').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Preparing download...');")
    exportData(forumdata)
    shinyjs::runjs("$('#downloadMFD')[0].click();") # DOWNLOAD BUTTON
    shinyjs::runjs("$('#btnGetMFD').text('Get Moodle forum messages');")
    #shinyjs::disable("btnGetMFD")
  })
  
  # downloadLogdata
  output$downloadMFD <- downloadHandler(
    filename = function() {
      fn <- genFileName(projname = config$projname, eventname = "forumdata", courseid = as.integer(input$selectLogdataCourse))
      if(input$selectLogdataOutputFormat == "CSV"){
        paste0(fn, ".csv")
      } else if(input$selectLogdataOutputFormat == "Parquet"){
        paste0(fn, ".parquet")
      } else if(input$selectLogdataOutputFormat == "Feather"){
        paste0(fn, ".feather")
      }
    },
    content = function(file) {
      if(input$selectOutputFormatMFD == "CSV"){
        utils::write.csv(exportData(), file, row.names = FALSE, quote = TRUE)
      } else if(input$selectOutputFormatMFD == "Parquet"){
        arrow::write_parquet(exportData(), file)
      } else if(input$selectOutputFormatMFD == "Feather"){
        arrow::write_feather(exportData(), file)
      }
    })

  # ---------------
  # Course Userlist
  # ---------------
  
  # observe Validator
  observeEvent(iv_cl$is_valid(), {
    if(iv_cl$is_valid()){
      shinyjs::enable("btnGetUserlist")
    }
    else{
      shinyjs::disable("btnGetUserlist")
    }
  })
  
  # observe btnGetUserList
  observeEvent(input$btnGetUserlist,{
      req(input$selectLogdataCourse)
      #userlist <- mdl_userlist(dbpMdl,config$dbprefix ,as.integer(input$selectLogdataCourse))
      
      tryCatch({
        userlist <- mdl_userlist(dbpMdl,config$dbprefix ,as.integer(input$selectLogdataCourse), returngroups = TRUE)
      }, warning = function(w) {
        showNotification('Could not retrieve user list.','',type = "error")
        return()
      }, error = function(e) {
        showNotification('Could not retrieve user list.','',type = "error")
        return()
      })
    
      # works up to here
      userlist <- userlist |> dplyr::select(username, groupname, grouping)
      userlist$hashuser <- pseudonymize(userlist$username, config$pepper)
      userlist$hashgroup <- pseudonymize(userlist$groupname, config$pepper)
      userlist <- userlist |> dplyr::select(hashuser, hashgroup, grouping)
      exportData(userlist)
      shinyjs::runjs("$('#downloadData')[0].click();") # DOWNLOAD BUTTON
    }
  )
  # end btnGetUserList
  
  # download userlist
  output$downloadData <- downloadHandler(
    filename = function() { 
      fn <- genFileName(projname = config$projname, eventname = "userlist", courseid = as.integer(input$selectLogdataCourse))
      paste0(fn, ".csv")
    },
    content = function(file) {
      utils::write.csv(exportData(), file, row.names = FALSE, quote = TRUE)
    })
  
  # course selector logic
  observeEvent(input$selectLogdataCourse, {
    updateNumericInput(session, "selectLogdataCourseID", value = as.numeric(input$selectLogdataCourse))
  })
  observeEvent(input$selectLogdataCourseID, {
    updateSelectInput(session, "selectLogdataCourse", selected = as.character(input$selectLogdataCourseID))
  })
  
  # -------------------------
  # Match & Pseudonymise data
  # -------------------------
  
  observeEvent(iv_mpd$is_valid(), {
    if(iv_mpd$is_valid()){
      shinyjs::enable("btnSurveyMatching")
    }
    else{
      shinyjs::disable("btnSurveyMatching")
    }
  })
  
  observeEvent(input$btnInspectSurveyData, {
    shiny::showModal(modalDialog(
      title = "Inspect data",
      size = "xl",
      bslib::card(
        shiny::tableOutput("tableSurveyData"),
        max_height = "480px"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$surveyData, {
    # load survey data
    
    #req(dfTemp) # would work if dfTemp was reactive
    dataFileExt <- tools::file_ext(input$surveyData$name)
    dataFileName <- tools::file_path_sans_ext(input$surveyData$name)
    tryCatch({
      if(tolower(dataFileExt) == "csv" ){
        dfTemp <- utils::read.csv(input$surveyData$datapath, sep = input$selectSurveyCSVSep)
      } else if(tolower(dataFileExt) == "sav"){
        dfTemp <- haven::read_sav(input$surveyData$datapath)
      } else if(tolower(dataFileExt) == "xlsx"){
        dfTemp <- openxlsx::read.xlsx(input$surveyData$datapath, sheet = 1)
      }
      
    }, error = function(e){
      
    })
    # enable inspect data
    shinyjs::enable("btnInspectSurveyData")
    output$tableSurveyData <- renderTable(utils::head(dfTemp, 50), 
                                striped = TRUE,
                                bordered = TRUE,
                                hover = TRUE,
                                spacing = "s"
    )
    
    importDataFilename <<- input$surveyData$name
    importData <<- dfTemp
    varnames <- colnames(dfTemp)
    shinyjs::reset("selectSurveyMatchingVar")
    shinyjs::reset("selectSurveyMatchingVarIs")
    shinyjs::reset("selectSurveyRemoveVars")
    updateSelectInput(session, "selectSurveyMatchingVar", choices = varnames)
    updateSelectInput(session, "selectSurveyRemoveVars", choices = varnames)
  })
  
  observeEvent(input$selectSurveyMatchingVar, {
    keepSel = input$selectSurveyRemoveVars
    updateSelectInput(session, "selectSurveyRemoveVars", 
                      choices = setdiff(colnames(importData), input$selectSurveyMatchingVar), selected = input$selectSurveyRemoveVars)
  })
  
  observeEvent(input$selectSurveyRemoveVars, {
    keepSel = input$selectSurveyMatchingVar
    updateSelectInput(session, "selectSurveyMatchingVar", 
                      choices = setdiff(colnames(importData), input$selectSurveyRemoveVars), selected = keepSel)
  })
  
  observeEvent(input$btnSurveyMatching, {
    req(dbpMdl)
    req(input$selectLogdataCourse)

    dta <- importData
    
    shinyjs::runjs("$('#btnSurveyMatching').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Matching data...');")
    matchingVar <- input$selectSurveyMatchingVar
    # change missing values to NA, so they don't get pseudonymised
    missingValues <- unlist(strsplit(input$txtSurveyMissingValues, ","))
    dta[matchingVar] <- lapply(dta[matchingVar], function(x) {
      x[x %in% missingValues] <- NA
      return(x)
    })
    
    # get data to match with
    shinyjs::runjs("$('#btnSurveyMatching').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Retrieving data to match with...');")
    
    if(input$selectSurveyMatchingVarIs == "Moodle-ID" | input$selectSurveyMatchingVarIs == "E-Mail"){
      tryCatch({
        matchlist <- mdl_userlist(dbpMdl,config$dbprefix ,as.integer(input$selectLogdataCourse))
      }, error = function(e) {
        showNotification('Could not retrieve user list.','',type = "error")
        return()
      }, finally = {
        shinyjs::runjs("$('#btnGetLogdata').text('Match & pseudonymise data');")
      })
      matchlist <- matchlist |> dplyr::select(id, email, username)
      matchlist$id <- as.integer(matchlist$id)
      matchlist$hashuser <- pseudonymize(matchlist$username, config$pepper)
      matchlist <- matchlist |> dplyr::select(-username) 
    }else if(input$selectSurveyMatchingVarIs == "Group-ID"){
      tryCatch({
        matchlist <- mdl_grouplist(dbpMdl,config$dbprefix ,as.integer(input$selectLogdataCourse))
      }, error = function(e) {
        showNotification('Could not retrieve group list.','',type = "error")
        return()
      }, finally = {
        shinyjs::runjs("$('#btnGetLogdata').text('Match & pseudonymise data');")
      })
      matchlist$hashgroup <- pseudonymize(matchlist$groupname, config$pepper)
      matchlist <- matchlist |> dplyr::select(-groupingid, -groupname) |> dplyr::select(groupid, hashgroup, groupingname)
    }
    
    # match data
    shinyjs::runjs("$('#btnSurveyMatching').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Matching data...');")
    
    if(input$selectSurveyMatchingVarIs == "Moodle-ID"){
      dta[[matchingVar]] <- as.integer(dta[[matchingVar]]) # ensure variables are numeric
      # filter matchlist for user-ids that are also in the dta data
      matchlist <- matchlist |> dplyr::select(-email) |> dplyr::filter(id %in% dta[[matchingVar]])
      # check if userlist = 0
      
      dta_out <- dplyr::left_join(dta, matchlist, by = setNames("id", matchingVar))
      # remove matchingVar
      dta_out <- dta_out |> dplyr::select(-matchingVar)
      # move hashuser to the front
      dta_out <- dta_out |> dplyr::select(hashuser, 1:(ncol(dta_out) - 1))
    }else if(input$selectSurveyMatchingVarIs == "Email"){
      # TODO
    }else if(input$selectSurveyMatchingVarIs == "Group-ID"){
      dta[[matchingVar]] <- as.integer(dta[[matchingVar]]) # ensure variables are numeric
      # filter matchlist for group-ids that are also in the dta data
      matchlist <- matchlist |> dplyr::filter(groupid %in% dta[[matchingVar]])
      # check if matchlist = 0
      
      dta_out <- dplyr::left_join(dta, matchlist, by = setNames("groupid", matchingVar))
      # remove matchingVar
      dta_out <- dta_out |> dplyr::select(-matchingVar)
      # move hashgroup to the front
      dta_out <- dta_out |> dplyr::select(hashgroup:groupingname, 1:(ncol(dta_out) - 1))
    }
    
    # remove additional vars from selectize list
    dta_out <- dta_out[, !names(dta_out) %in% input$selectSurveyRemoveVars]
    # set variable for data export
    shinyjs::runjs("$('#btnSurveyMatching').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Preparing download...');")
    exportData(shuffle(shuffle(dta_out)))
    shinyjs::runjs("$('#downloadMatchdata')[0].click();") # DOWNLOAD BUTTON
    shinyjs::runjs("$('#btnSurveyMatching').text('Match & pseudonymise data');")
    
    # reset UI
    importData <- NULL
    shinyjs::disable("btnSurveyMatching")
    shinyjs::disable("btnInspectSurveyData")
    shinyjs::reset("surveyData")
    shinyjs::reset("txtSurveyMissingValues")
    shinyjs::reset("selectSurveyMatchingVar")
    shinyjs::reset("selectSurveyMatchingVarIs")
    shinyjs::reset("selectSurveyRemoveVars")
  })
  
  # download matched data
  output$downloadMatchdata <- downloadHandler(
    filename = function() {
      dlExt <- tools::file_ext(importDataFilename)
      dlName <- tools::file_path_sans_ext(importDataFilename)
      paste0(dlName, "_anon", ".", dlExt)
    },
    content = function(file) {
      dlExt <- tools::file_ext(importDataFilename)
      if(tolower(dlExt) == "csv" ){
        utils::write.csv(exportData(), file, row.names = FALSE, quote = TRUE)
      } else if(tolower(dlExt) == "sav"){
        haven::write_sav(exportData(), file, compress = "zsav")
      } else if(tolower(dlExt) == "xlsx"){
        openxlsx::write.xlsx(exportData(), file)
      }
      exportData(NULL)
    })
  
  # -----------------
  # pseudonymise data
  # -----------------
  
  observeEvent(iv_pd$is_valid(), {
    if(iv_pd$is_valid() & iv_pepper$is_valid()){
      shinyjs::enable("btnPseudData")
    }
    else{
      shinyjs::disable("btnPseudData")
    }
  })
  
  observeEvent(input$pseudData, {
    # load data
    #req(dfTemp) # would work if dfTemp was reactive
    dataFileExt <- tools::file_ext(input$pseudData$name)
    dataFileName <- tools::file_path_sans_ext(input$pseudData$name)
    tryCatch({
      if(tolower(dataFileExt) == "csv" ){
        dfTemp <- utils::read.csv(input$pseudData$datapath, sep = input$selectPseudDataCSVSep)
      } else if(tolower(dataFileExt) == "sav"){
        dfTemp <- haven::read_sav(input$pseudData$datapath)
      } else if(tolower(dataFileExt) == "xlsx"){
        dfTemp <- openxlsx::read.xlsx(input$pseudData$datapath, sheet = 1)
      }
    }, error = function(e){
      
    })
    
    importData <<- NULL
    shinyjs::reset("selectPseudDataVars")
    # enable inspect data
    shinyjs::enable("btnInspectPseudData")
    output$table <- renderTable(utils::head(dfTemp, 50), 
                                striped = TRUE,
                                bordered = TRUE,
                                hover = TRUE,
                                spacing = "s"
    )
    
    importDataFilename <<- input$pseudData$name
    importData <<- dfTemp
    varnames <- colnames(dfTemp)
    updateSelectInput(session, "selectPseudDataVars", choices = varnames)
  })
  
  observeEvent(input$btnInspectPseudData, {
    shiny::showModal(modalDialog(
      title = "Inspect data",
      size = "xl",
      bslib::card(
        shiny::tableOutput("table"),
        max_height = "480px"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$btnPseudData, {
    #req(dbpMdl)
    #req(input$selectLogdataCourse)
    req(config)
    req(importData)
    dta <- importData
    # pseudonymise data
    shinyjs::runjs("$('#btnPseudData').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Pseudonymising data...');")
    pseudVars <- input$selectPseudDataVars
    # change missing values to NA, so they don't get pseudonymised
    missingValues <- unlist(strsplit(input$txtPseudDataMissingValues, ","))
    dta[pseudVars] <- lapply(dta[pseudVars], function(x) {
      x[x %in% missingValues] <- NA
      return(x)
    })
    # call pseudonymise
    dta[pseudVars] <- lapply(dta[pseudVars], function(x) sapply(x, pseudonymize, salt = config$pepper))
    
    # remove variables
    dta <- dta[, !names(dta) %in% input$selectPseudDataRemoveVars]
  
    # set variable for data export
    shinyjs::runjs("$('#btnPseudData').html('<i class=\"fa-solid fa-sync fa-spin\"></i> Preparing download...');")
    exportData(shuffle(shuffle(dta)))
    shinyjs::runjs("$('#downloadPseuddata')[0].click();") # DOWNLOAD BUTTON
    shinyjs::runjs("$('#btnPseudData').text('Pseudonymise data');")
    importData <- NULL
    shinyjs::disable("btnPseudData")
    shinyjs::disable("btnInspectPseudData")
    shinyjs::reset("txtPseudDataMissingValues")
    shinyjs::reset("pseudData")
    shinyjs::reset("selectPseudDataVars")
    shinyjs::reset("selectPseudDataRemoveVars")
  })
  
  observeEvent(input$selectPseudDataVars, {
    keepSel = input$selectSurveyRemoveVars
    updateSelectInput(session, "selectPseudDataRemoveVars", 
                      choices = setdiff(colnames(importData), input$selectPseudDataVars), selected = input$selectSurveyRemoveVars)
  })
  
  observeEvent(input$selectPseudDataRemoveVars, {
    keepSel = input$selectPseudDataVars
    updateSelectInput(session, "selectPseudDataVars", 
                      choices = setdiff(colnames(importData), input$selectPseudDataRemoveVars), selected = keepSel)
  })
  
  output$downloadPseuddata <- downloadHandler(
    filename = function() {
      dlExt <- tools::file_ext(importDataFilename)
      dlName <- tools::file_path_sans_ext(importDataFilename)
      paste0(dlName, "_anon", ".", dlExt)
    },
    content = function(file) {
      dlExt <- tools::file_ext(importDataFilename)
      if(tolower(dlExt) == "csv" ){
        utils::write.csv(exportData(), file, row.names = FALSE, quote = TRUE)
      } else if(tolower(dlExt) == "sav"){
        haven::write_sav(exportData(), file, compress = "zsav")
      } else if(tolower(dlExt) == "xlsx"){
        openxlsx::write.xlsx(exportData(), file)
      }
      exportData(NULL)
    })
  
  # handling end of session
  session$onSessionEnded(function() {
    #print(dbpMdl)
    if (!is.null(dbpMdl) && inherits(dbpMdl, "Pool")) {
      tryCatch({
        pool::poolClose(dbpMdl)
        message("Database pool object closed.")
      }, error = function(e) {
        message("Error returning the pool: ", e$message)
      })
    } else {
      #message("Pool is NULL or not a pooled object.")
      message("No pool object to return. You are set.")
    }
    print("Thank you for using moodleLA and goodbye.")
  })
  
} # end server.ui

# Run the application 
app <- shinyApp(ui, server, enableBookmarking = "disable")

} # end function moodleLA
