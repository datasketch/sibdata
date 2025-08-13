# app3.R
# Modular version of SIB Data App - Recreated from scratch

# Debug configuration
DEBUG_MODE <- FALSE  # Set to FALSE to hide debug output
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
             exp_debug_ui("debug"),
             h4("Opciones"),
             exp_inputs_ui("inputs")
           )
    ),

    # Center column - Visualization (50%)
    column(6, style = "padding: 0 5px;",
           wellPanel(
             h4("Visualización"),
             exp_visualization3_ui("visualization"),
             # Add debug2 below visualization when debug mode is on
             exp_debug2_ui("debug2")
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
  con <- get_app_connection("db/sibdata.sqlite", debug = DEBUG_MODE)
  app_options <- get_app_options(con, debug = DEBUG_MODE)
  app_options$con <- con
  conmap <- geotable::gt_con()


  # Create session-specific reactive values
  r <- reactiveValues(
    sel_region = NULL,
    sel_grupo_tipo = "biologico",
    sel_grupo = NULL,
    sel_tematica = NULL,
    tematica = NULL,
    sel_tipo = "registros",
    chart_type = "map",
    is_special_region = FALSE,
    has_subtematica = FALSE,
    inputs_ready = NULL,
    # Data controls
    amenazadas_categoria = NULL,
    cites_categoria = NULL,
    exotica_categoria = NULL,
    especies_total_estimadas = NULL,
    # Chart selector
    available_charts = c("Mapa" = "map", "Tabla" = "table"),
    indicador = NULL,
    breadcrumb = NULL,
    # Data storage for charts and modals
    main_data = NULL,
    map_data = NULL,
    table_data = NULL,
    chart_data = NULL,
    current_chart_data = NULL,
    # Error handling
    viz_error = NULL,
    # Database connections
    con = con,
    conmap = conmap
  )



  # Initialize modules
  if (DEBUG_MODE) message("📦 INITIALIZING MODULES")
  exp_inputs_server("inputs", r, app_options, session, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Inputs module initialized")

  exp_visualization3_server("visualization", r, con, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Visualization module initialized")

  exp_species_table_server("species", r, con, session, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Species table module initialized")

  exp_debug_server("debug", r, debug = FALSE)
  if (DEBUG_MODE) message("✓ Debug module initialized")

  exp_debug2_server("debug2", r, debug = FALSE)
  if (DEBUG_MODE) message("✓ Debug2 module initialized")

  # Close database connection when session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(conmap)
  })
}

shinyApp(ui, server)
