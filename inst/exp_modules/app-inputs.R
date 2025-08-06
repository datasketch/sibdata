# Test app for exp_inputs module
# Tests the integration with exp_inputs_grupo and exp_inputs_tematica modules

library(shiny)
library(shinyjs)
library(sibdata)

# Get database connection
con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)

# Get app options (same as in app2.R)
app_options <- get_app_options(con, debug = TRUE)

# Add the connection to app_options (like app2.R does)
app_options$con <- con

###### APP
# Test UI - similar to app2.R but focused on inputs
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Test exp_inputs Module with Nested Submodules"),
  
  fluidRow(
    # Left column - Input controls (50%)
    column(6, style = "padding: 0 5px;",
           wellPanel(
             h4("Input Controls"),
             exp_inputs_ui("test_inputs")
           )
    ),
    
    # Right column - Debug output (50%)
    column(6, style = "padding: 0 5px;",
           wellPanel(
             h4("Debug Output"),
             verbatimTextOutput("debug_output"),
             hr(),
             h5("Current Grupo Selection:"),
             textOutput("current_grupo"),
             hr(),
             h5("Current Tematica Selection:"),
             textOutput("current_tematica"),
             hr(),
             h5("Reactive Values:"),
             verbatimTextOutput("reactive_values"),
             hr(),
             h5("URL Parameters:"),
             verbatimTextOutput("url_params"),
             hr(),
             h5("Module Namespace Debug:"),
             verbatimTextOutput("namespace_debug")
           )
    )
  )
)

# Test server
server <- function(input, output, session) {
  
  # Create reactive values object (same as in app2.R)
  r <- reactiveValues(
    sel_region = NULL,
    sel_grupo_type = "biologico",
    sel_grupo = NULL,
    sel_tematica = NULL,
    sel_tipo = "registros",
    chart_type = "map"
  )
  
  # Initialize the inputs module
  exp_inputs_server("test_inputs", r, app_options, session, debug = TRUE)
  
  # Create a reactive expression to track all reactive values
  reactive_values_tracker <- reactive({
    list(
      sel_region = r$sel_region,
      sel_grupo_type = r$sel_grupo_type,
      sel_grupo = r$sel_grupo,
      sel_tematica = r$sel_tematica,
      sel_tipo = r$sel_tipo,
      chart_type = r$chart_type
    )
  })
  
  # Debug output
  output$debug_output <- renderPrint({
    cat("=== DEBUG OUTPUT ===\n")
    cat("Timestamp:", Sys.time(), "\n")
    cat("App options loaded:", !is.null(app_options), "\n")
    if (!is.null(app_options)) {
      cat("Available regions:", length(app_options$region), "\n")
      cat("Available biological groups:", length(app_options$grupo_biologico), "\n")
      cat("Available interest groups:", length(app_options$grupo_interes), "\n")
      cat("Database connection in app_options:", !is.null(app_options$con), "\n")
    }
    cat("Database connection active:", DBI::dbIsValid(con), "\n")
    cat("===================\n")
  })
  
  # Current grupo selection
  output$current_grupo <- renderText({
    grupo_type <- r$sel_grupo_type
    grupo_value <- r$sel_grupo
    
    if (is.null(grupo_type) || is.null(grupo_value)) {
      "No grupo selected"
    } else {
      paste("Selected:", grupo_value, "(", grupo_type, ")")
    }
  })
  
  # Current tematica selection
  output$current_tematica <- renderText({
    tematica <- r$sel_tematica
    if (is.null(tematica) || tematica == "") {
      "No tematica selected"
    } else {
      paste("Selected:", tematica)
    }
  })
  
  # Reactive values output
  output$reactive_values <- renderPrint({
    # Force reactivity by accessing the tracker
    tracker <- reactive_values_tracker()
    
    cat("=== REACTIVE VALUES ===\n")
    cat("Timestamp:", Sys.time(), "\n")
    cat("sel_region:", r$sel_region, "\n")
    cat("sel_grupo_type:", r$sel_grupo_type, "\n")
    cat("sel_grupo:", r$sel_grupo, "\n")
    cat("sel_tematica:", r$sel_tematica, "\n")
    cat("sel_tipo:", r$sel_tipo, "\n")
    cat("chart_type:", r$chart_type, "\n")
    cat("=====================\n")
  })
  
  # URL parameters output
  output$url_params <- renderPrint({
    cat("=== URL PARAMETERS ===\n")
    query <- parseQueryString(session$clientData$url_search)
    if (length(query) > 0) {
      for (param in names(query)) {
        cat(param, ":", query[[param]], "\n")
      }
    } else {
      cat("No URL parameters found\n")
    }
    cat("=====================\n")
  })
  
  # Namespace debug output
  output$namespace_debug <- renderPrint({
    cat("=== NAMESPACE DEBUG ===\n")
    cat("Main session namespace test:", session$ns("test"), "\n")
    cat("Expected grupo namespace:", session$ns("test_inputs-grupo"), "\n")
    cat("Expected tematica namespace:", session$ns("test_inputs-tematica"), "\n")
    
    # Check if inputs exist
    cat("\nInput existence check:\n")
    cat("sel_region exists:", !is.null(input$sel_region), "\n")
    
    # Check for grupo-related inputs
    grupo_inputs <- c("grupo_biologico", "grupo_interes", "grupo_biologico_children", "grupo_interes_children")
    for (input_name in grupo_inputs) {
      full_id <- paste0("test_inputs-", input_name)
      cat(full_id, "exists:", !is.null(input[[input_name]]), "\n")
    }
    
    # Check for tematica-related inputs (we'll check a few common ones)
    tematica_inputs <- c("amenazadas", "amenazadas_children", "biologico", "biologico_children")
    for (input_name in tematica_inputs) {
      full_id <- paste0("test_inputs-", input_name)
      cat(full_id, "exists:", !is.null(input[[input_name]]), "\n")
    }
    
    cat("=====================\n")
  })
  
  # Close database connection when session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
}

# Run the app
shinyApp(ui = ui, server = server) 