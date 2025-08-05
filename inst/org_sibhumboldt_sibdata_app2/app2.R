# app2.R
# Modular version of SIB Data App

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

# Source debug module
source("exp_debug.R")

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
             exp_visualization_ui("viz")
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

  exp_visualization_server("viz", r, con, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Visualization module initialized")

  exp_debug_server("debug", r, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Debug module initialized")

  # Add dropdown click-outside behavior
  observe({
    shinyjs::runjs("
      // Remove any existing event listeners
      document.removeEventListener('click', window.dropdownClickHandler);

      // Create new click handler
      window.dropdownClickHandler = function(event) {
        const dropdowns = document.querySelectorAll('.dropdown-details[open]');
        dropdowns.forEach(function(dropdown) {
          if (!dropdown.contains(event.target)) {
            dropdown.removeAttribute('open');
          }
        });
      };

      // Add event listener
      document.addEventListener('click', window.dropdownClickHandler);
    ")
  })

  # Central reactive conditions - chart availability and control visibility
  observe({
    if (DEBUG_MODE) message("🔧 CENTRAL REACTIVE CONDITIONS")

    # Helper function for special themes
    is_amenazadas_or_cites_or_exoticas <- function() {
      tematica <- r$sel_tematica
      if(is.null(tematica)) return(FALSE)
      (grepl("cites", tematica) || grepl("amenazadas", tematica) || grepl("exoticas", tematica))
    }

    # 1. Chart availability logic (from original app lines 340-355)
    if(!is_amenazadas_or_cites_or_exoticas()) {
      if(r$sel_tipo == "registros") {
        r$available_charts <- c("Mapa" = "map", "Tabla" = "table")
      } else if(r$sel_tipo == "especies") {
        r$available_charts <- c("Mapa" = "map", "Tabla" = "table", "Barras" = "bar")
      }
    } else {
      # For amenazadas/cites/exoticas: ALL charts available
      r$available_charts <- c("Mapa" = "map", "Torta" = "pie", "Dona" = "donut",
                             "Treemap" = "treemap", "Barras" = "bar", "Tabla" = "table")
    }

    # 2. Control visibility logic (CORRECTED from original app)
    # Show subcategory controls ONLY for MAP charts in amenazadas themes
    # Non-map charts (bar, pie, donut, treemap) compare subcategories, so no filter needed
    r$show_subcategoria <- is_amenazadas_or_cites_or_exoticas() && r$chart_type == "map"

    # Species total/estimadas only for map + especies + regular theme
    r$show_especies_total_estimadas <- r$chart_type == "map" &&
      r$sel_tipo == "especies" &&
      !is_amenazadas_or_cites_or_exoticas() &&
      is.null(r$sel_tematica)

    # Ensure current chart is available
    if(!is.null(r$chart_type) && !is.null(r$available_charts)) {
      if(!r$chart_type %in% r$available_charts) {
        old_chart <- r$chart_type
        r$chart_type <- r$available_charts[1]
        if (DEBUG_MODE) message("Chart type changed due to availability: ", old_chart, " -> ", r$chart_type)
      }
    }

    if (DEBUG_MODE) {
      message("✓ Available charts: ", paste(names(r$available_charts), collapse = ", "))
      message("✓ Show subcategoria: ", r$show_subcategoria)
      message("✓ Show especies total/estimadas: ", r$show_especies_total_estimadas)
    }
  })

  # Central data management - compute indicator and fetch main data
  observe({
    if (DEBUG_MODE) {
      message("🔍 DATA OBSERVER TRIGGERED")
      message("Current reactive values:")
      message("- r$sel_region: ", r$sel_region)
      message("- r$sel_tipo: ", r$sel_tipo)
      message("- r$chart_type: ", r$chart_type)
      message("- r$indicador: ", r$indicador)
    }

    req(r$sel_region)
    if (DEBUG_MODE) message("✓ r$sel_region requirement met: ", r$sel_region)

    req(r$sel_tipo)
    if (DEBUG_MODE) message("✓ r$sel_tipo requirement met: ", r$sel_tipo)

    req(r$chart_type)
    if (DEBUG_MODE) message("✓ r$chart_type requirement met: ", r$chart_type)

    if (DEBUG_MODE) message("=== COMPUTING INDICATOR ===")

    # Helper function for special themes
    is_amenazadas_or_cites_or_exoticas <- function() {
      tematica <- r$sel_tematica
      if(is.null(tematica)) return(FALSE)
      (grepl("cites", tematica) || grepl("amenazadas", tematica))
    }

    old_indicador <- r$indicador

    # Compute indicator for maps
    if(r$chart_type == "map") {
      if (DEBUG_MODE) message("Chart type is map - computing indicator...")
      if(is_amenazadas_or_cites_or_exoticas()) {
        if (DEBUG_MODE) message("Special theme detected")
        if(!is.null(r$sel_tematica) && grepl("amenazadas", r$sel_tematica)) {
          # Use subcategory if available
          subcategory <- r$amenazadas_categoria %||% "_total"
          # Convert dashes to underscores for API compatibility
          tematica_api <- gsub("-", "_", r$sel_tematica)
          r$indicador <- paste0(r$sel_tipo, "_", tematica_api, subcategory)
        } else if(!is.null(r$sel_tematica) && grepl("cites", r$sel_tematica)) {
          # Use subcategory if available
          subcategory <- r$cites_categoria %||% "_total"
          # Convert dashes to underscores for API compatibility
          tematica_api <- gsub("-", "_", r$sel_tematica)
          r$indicador <- paste0(r$sel_tipo, "_", tematica_api, subcategory)
        }
      } else {
        if (DEBUG_MODE) message("Regular theme")
        if(r$sel_tipo == "especies" && is.null(r$sel_tematica)) {
          # Use total/estimadas if available
          total_est <- r$especies_total_estimadas %||% "total"
          r$indicador <- paste0(r$sel_tipo, "_region_", total_est)
        } else {
          r$indicador <- NULL
        }
      }
    } else {
      if (DEBUG_MODE) message("Chart type is not map - setting indicador to NULL")
      r$indicador <- NULL
    }

    if (DEBUG_MODE) message("Indicador changed from '", old_indicador, "' to '", r$indicador, "'")

    # Fetch main data
    tryCatch({
      fetch_start <- Sys.time()
      if (DEBUG_MODE) {
        message("=== STARTING DATA FETCH ===")
        message("Fetch started at: ", fetch_start)
        message("Parameters:")
        message("- Region: ", r$sel_region)
        message("- Grupo: ", r$sel_grupo)
        message("- Tipo: ", r$sel_tipo)
        message("- Tematica: ", r$sel_tematica)
        message("- Indicador: ", r$indicador)
        message("- Chart type: ", r$chart_type)
        message("- Subregiones: ", if(r$chart_type == "map") TRUE else FALSE)
      }

      # Convert dashes to underscores for API compatibility
      tematica_api <- if(!is.null(r$sel_tematica)) gsub("-", "_", r$sel_tematica) else r$sel_tematica

      d <- sibdata(
        region = r$sel_region,
        grupo = r$sel_grupo,
        tipo = r$sel_tipo,
        tematica = tematica_api,
        indicador = r$indicador,
        subregiones = if(r$chart_type == "map") TRUE else FALSE,
        with_parent = FALSE,
        con = con
      )

      fetch_end <- Sys.time()
      fetch_duration <- difftime(fetch_end, fetch_start, units = "secs")

      if (DEBUG_MODE) {
        message("✓ DATA FETCH COMPLETED in ", round(fetch_duration, 2), " seconds")
        message("✓ Data rows: ", nrow(d))
        message("✓ Data columns: ", paste(names(d), collapse = ", "))

        # Debug: Print raw data structure
        message("📊 RAW DATA DEBUG:")
        if(nrow(d) > 0) {
          for(i in 1:min(3, nrow(d))) {
            row_data <- sapply(d[i,], function(x) if(is.null(x)) "NULL" else as.character(x))
            message("Row ", i, ": ", paste(names(d), "=", row_data, collapse = " | "))
          }
        }
      }

      # For charts (not map/table), merge indicator labels
      if(r$chart_type %in% c("pie", "donut", "treemap", "bar")) {
        if (DEBUG_MODE) message("Merging indicator labels for chart type: ", r$chart_type)
        d_before <- d
        d <- d |> sib_merge_ind_label(con = con)
        if (DEBUG_MODE) message("✓ Indicator labels merged")

        # Debug: Print processed data structure
        if (DEBUG_MODE) {
          message("📊 PROCESSED DATA DEBUG:")
          if(nrow(d) > 0) {
            for(i in 1:min(3, nrow(d))) {
              row_data <- sapply(d[i,], function(x) if(is.null(x)) "NULL" else as.character(x))
              message("Row ", i, ": ", paste(names(d), "=", row_data, collapse = " | "))
            }
          }
        }
      }

      old_data_rows <- if(is.null(r$main_data)) 0 else nrow(r$main_data)
      r$main_data <- d
      if (DEBUG_MODE) message("✓ r$main_data updated (was ", old_data_rows, " rows, now ", nrow(d), " rows)")

    }, error = function(e) {
      if (DEBUG_MODE) {
        message("❌ ERROR fetching main data:")
        message("Error message: ", e$message)
        message("Error details: ", conditionMessage(e))
      }
      r$main_data <- NULL
      if (DEBUG_MODE) message("✓ r$main_data set to NULL due to error")
    })

    if (DEBUG_MODE) message("🏁 DATA OBSERVER COMPLETED")
  })



  # Close database connection when session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
}

shinyApp(ui, server)



