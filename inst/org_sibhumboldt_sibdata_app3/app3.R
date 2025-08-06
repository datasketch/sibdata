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

# Debug module is available from sibdata package

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),

  fluidRow(
    # Left column - Input controls (25%)
    column(3, style = "padding: 0 5px;",
           wellPanel(
             exp_debug_ui("debug", debug = DEBUG_MODE),
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

    # Right column - Species table (25%)
    column(3, style = "padding: 0 5px;",
           wellPanel(
             exp_species_table_ui("species")
           )
    )
  )
)

server <- function(input, output, session) {

  if (DEBUG_MODE) message("🚀 SERVER STARTING")

  # Create session-specific app options
  temp_con <- get_app_connection("db/sibdata.sqlite", debug = DEBUG_MODE)
  app_options <- get_app_options(temp_con, debug = DEBUG_MODE)
  DBI::dbDisconnect(temp_con)
  if (DEBUG_MODE) message("✓ App options loaded")

  # Create database connection inside server
  con <- get_app_connection("db/sibdata.sqlite", debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Database connection created")

  # Add the connection to app_options for the tematica module
  app_options$con <- con

  # Create session-specific reactive values
  r <- reactiveValues(
    sel_region = NULL,
    sel_grupo_type = "biologico",
    sel_grupo = NULL,
    sel_tematica = NULL,
    sel_tipo = "registros",
    chart_type = "map",
    amenazadas_categoria = NULL,
    cites_categoria = NULL,
    exotica_categoria = NULL,
    especies_total_estimadas = NULL,
    indicador = NULL,
    show_subcategoria = FALSE,
    show_especies_total_estimadas = FALSE,
    current_subcategory = NULL,
    main_data = NULL,
    species_data = NULL,
    map_data = NULL,
    breadcrumb = NULL,
    available_charts = NULL
  )

  # Debug: Verify database tables exist
  tryCatch({
    tables <- DBI::dbListTables(con)
    if (DEBUG_MODE) message("📊 Available database tables: ", paste(tables, collapse = ", "))

    # Check for critical tables
    required_tables <- c("especie_region", "ind_meta", "indicadores")
    missing_tables <- required_tables[!required_tables %in% tables]
    if(length(missing_tables) > 0) {
      if (DEBUG_MODE) message("❌ Missing required tables: ", paste(missing_tables, collapse = ", "))
    } else {
      if (DEBUG_MODE) message("✅ All required tables found")
    }
  }, error = function(e) {
    if (DEBUG_MODE) message("❌ Error checking database tables: ", e$message)
  })

  # Initialize modules
  if (DEBUG_MODE) message("📦 INITIALIZING MODULES")
  exp_inputs_server("inputs", r, app_options, session, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Inputs module initialized")

  exp_species_table_server("species", r, con, session, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Species table module initialized")

  exp_debug_server("debug", r, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Debug module initialized")

  # Close database connection when session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
}

shinyApp(ui, server)
