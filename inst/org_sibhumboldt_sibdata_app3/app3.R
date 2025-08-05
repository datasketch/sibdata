# app3.R
# Modular version of SIB Data App - Recreated from scratch

# Debug configuration
DEBUG_MODE <- TRUE  # Set to FALSE to hide debug output
options(timeout = 600)

library(shiny)
library(DT)
library(tidyverse)
library(sibdata)
library(data.tree)
library(htmlwidgets)
library(leaflet)
library(geotable)
library(leaflet.extras)
library(openxlsx)
library(jsonlite)
library(highcharter)
library(hgmagic)
library(shinyinvoer)
library(shinyjs)

# Get database connection (same as app-inputs.R)
con <- get_app_connection(NULL, debug = DEBUG_MODE)

# Get app options (same as app-inputs.R)
app_options <- get_app_options(con, debug = DEBUG_MODE)

# Add the connection to app_options for the tematica module
app_options$con <- con

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),

  fluidRow(
    # Left column - Input controls (25%)
    column(3, style = "padding: 0 5px;",
           wellPanel(
             h4("Opciones"),
             exp_inputs_ui("inputs")
           )
    ),

    # Center column - Visualization (50%)
    column(6, style = "padding: 0 5px;",
           wellPanel(
             h4("Visualización"),
             # Placeholder for visualization module
             p("Visualization module will be added here")
           )
    ),

    # Right column - Debug output (25%)
    column(3, style = "padding: 0 5px;",
           wellPanel(
             h4("Debug Output"),
             verbatimTextOutput("debug_output"),
             hr(),
             h5("Current Tematica Selection:"),
             textOutput("current_tematica"),
             hr(),
             h5("Reactive Values:"),
             verbatimTextOutput("reactive_values")
           )
    )
  )
)

server <- function(input, output, session) {

  if (DEBUG_MODE) message("🚀 SERVER STARTING")

  # Create session-specific reactive values (same as app-inputs.R)
  r <- reactiveValues(
    sel_region = NULL,
    sel_grupo_type = "biologico",
    sel_grupo = NULL,
    sel_tematica = NULL,
    sel_tipo = "registros",
    chart_type = "map"
  )

  # Debug: Verify database tables exist
  tryCatch({
    tables <- DBI::dbListTables(con)
    if (DEBUG_MODE) message("📊 Available database tables: ", paste(tables, collapse = ", "))

    # Check for critical tables
    required_tables <- c("especie_region", "ind_meta", "indicadores", "tematica")
    missing_tables <- required_tables[!required_tables %in% tables]
    if(length(missing_tables) > 0) {
      if (DEBUG_MODE) message("❌ Missing required tables: ", paste(missing_tables, collapse = ", "))
    } else {
      if (DEBUG_MODE) message("✅ All required tables found")
    }
  }, error = function(e) {
    if (DEBUG_MODE) message("❌ Error checking database tables: ", e$message)
  })

  # Initialize inputs module
  if (DEBUG_MODE) message("📦 INITIALIZING INPUTS MODULE")
  exp_inputs_server("inputs", r, app_options, session, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Inputs module initialized")

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

  # Close database connection when session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
}

shinyApp(ui, server) 