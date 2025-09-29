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

# Optional dependency for chart image export
if (requireNamespace("webshot", quietly = TRUE)) {
  library(webshot)
}

# Debug module is available from sibdata package

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css"),
    # Loading spinner CSS
    tags$style(HTML("
      .loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(255, 255, 255, 0.9);
        z-index: 9999;
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
      }

      .spinner {
        border: 4px solid #f3f3f3;
        border-top: 4px solid #09A274;
        border-radius: 50%;
        width: 50px;
        height: 50px;
        animation: spin 1s linear infinite;
      }

      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }

      .loading-text {
        margin-top: 20px;
        font-size: 16px;
        color: #09A274;
        font-weight: 500;
      }

      .section-loading {
        position: relative;
        opacity: 0.6;
      }

      .section-loading::after {
        content: '';
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        border: 3px solid #f3f3f3;
        border-top: 3px solid #09A274;
        border-radius: 50%;
        width: 30px;
        height: 30px;
        animation: spin 1s linear infinite;
        z-index: 1000;
      }
    "))
  ),

  # Global loading overlay
  div(id = "global-loading", class = "loading-overlay", style = "display: none;",
      div(class = "spinner"),
      div(id = "loading-text", class = "loading-text", "Cargando aplicación...")
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

  # Show loading on app start
  shinyjs::show("global-loading")

  # Helper functions for loading management
  show_loading <- function(text = "Cargando...") {
    shinyjs::html("loading-text", text)
    shinyjs::show("global-loading")
  }

  hide_loading <- function() {
    shinyjs::hide("global-loading")
  }

  # Create session-specific app options
  # con <- get_app_connection("db/sibdata.sqlite", debug = DEBUG_MODE)
  con <- get_app_connection("db/sibdata.duckdb", debug = DEBUG_MODE)
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
    # Chart selector - include cards first by default
    available_charts = c("Tarjetas" = "cards", "Mapa" = "map", "Tabla" = "table"),
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

  exp_visualization3_server("visualization", r, con,
                            loading_fns = list(show = show_loading, hide = hide_loading),
                            debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Visualization module initialized")

  exp_species_table_server("species", r, con, session,
                           loading_fns = list(show = show_loading, hide = hide_loading),
                           debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Species table module initialized")

  exp_debug_server("debug", r, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Debug module initialized")

  exp_debug2_server("debug2", r, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Debug2 module initialized")

  # Hide loading spinner after all modules are initialized
  observe({
    # Wait for initial data to be ready
    req(r$inputs_ready)
    req(r$sel_region)

    # Small delay to ensure everything is rendered
    shinyjs::delay(500, {
      hide_loading()
      if (DEBUG_MODE) message("✓ App fully loaded, hiding spinner")
    })
  })

  # Close database connection when session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(conmap)
  })
}

shinyApp(ui, server)
