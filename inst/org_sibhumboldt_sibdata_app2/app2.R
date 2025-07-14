# app2.R
# Modular version of SIB Data App

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


# Initialize app options (we'll create connection in server)
# Temporary connection just for getting options
temp_con <- get_app_connection("db/sibdata.sqlite")
app_options <- get_app_options(temp_con)
DBI::dbDisconnect(temp_con)

# Centralized reactive values
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

ui <- fluidPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),

  fluidRow(
    # Left column - Input controls
    column(3,
           wellPanel(
             h4("Debug - Reactive Values"),
             div(class = "debug-container",
                 verbatimTextOutput("debug_reactive")
             ),
             hr(),
             h4("Opciones"),
             exp_inputs_ui("inputs")
           )
    ),

    # Center column - Visualization
    column(6,
           wellPanel(
             h4("Visualización"),
             exp_visualization_ui("viz")
           )
    ),

    # Right column - Species table
    column(3,
           wellPanel(
             exp_species_table_ui("species")
           )
    )
  )
)

server <- function(input, output, session) {
  message("🚀 SERVER STARTING")

  # Create database connection inside server
  con <- get_app_connection()
  message("✓ Database connection created")

  # Initialize default values
  message("🔧 INITIALIZING DEFAULT VALUES")
  message("Setting defaults: region=colombia, tipo=registros, chart_type=map")
  r$sel_region <- "colombia"
  r$sel_tipo <- "registros"
  r$chart_type <- "map"
  message("✓ Default values set")

  # Initialize modules
  message("📦 INITIALIZING MODULES")
  exp_inputs_server("inputs", r, app_options, session)
  message("✓ Inputs module initialized")

  exp_species_table_server("species", r, con)
  message("✓ Species table module initialized")

  exp_visualization_server("viz", r, con)
  message("✓ Visualization module initialized")

  # Central reactive conditions - chart availability and control visibility
  observe({
    message("🔧 CENTRAL REACTIVE CONDITIONS")

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
        message("Chart type changed due to availability: ", old_chart, " -> ", r$chart_type)
      }
    }

    message("✓ Available charts: ", paste(names(r$available_charts), collapse = ", "))
    message("✓ Show subcategoria: ", r$show_subcategoria)
    message("✓ Show especies total/estimadas: ", r$show_especies_total_estimadas)
  })

  # Central data management - compute indicator and fetch main data
  observe({
    message("🔍 DATA OBSERVER TRIGGERED")
    message("Current reactive values:")
    message("- r$sel_region: ", r$sel_region)
    message("- r$sel_tipo: ", r$sel_tipo)
    message("- r$chart_type: ", r$chart_type)
    message("- r$indicador: ", r$indicador)

    req(r$sel_region)
    message("✓ r$sel_region requirement met: ", r$sel_region)

    req(r$sel_tipo)
    message("✓ r$sel_tipo requirement met: ", r$sel_tipo)

    req(r$chart_type)
    message("✓ r$chart_type requirement met: ", r$chart_type)

    message("=== COMPUTING INDICATOR ===")

    # Helper function for special themes
    is_amenazadas_or_cites_or_exoticas <- function() {
      tematica <- r$sel_tematica
      if(is.null(tematica)) return(FALSE)
      (grepl("cites", tematica) || grepl("amenazadas", tematica))
    }

    old_indicador <- r$indicador

    # Compute indicator for maps
    if(r$chart_type == "map") {
      message("Chart type is map - computing indicator...")
      if(is_amenazadas_or_cites_or_exoticas()) {
        message("Special theme detected")
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
        message("Regular theme")
        if(r$sel_tipo == "especies" && is.null(r$sel_tematica)) {
          # Use total/estimadas if available
          total_est <- r$especies_total_estimadas %||% "total"
          r$indicador <- paste0(r$sel_tipo, "_region_", total_est)
        } else {
          r$indicador <- NULL
        }
      }
    } else {
      message("Chart type is not map - setting indicador to NULL")
      r$indicador <- NULL
    }

    message("Indicador changed from '", old_indicador, "' to '", r$indicador, "'")

    # Fetch main data
    tryCatch({
      fetch_start <- Sys.time()
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

      # For charts (not map/table), merge indicator labels
      if(r$chart_type %in% c("pie", "donut", "treemap", "bar")) {
        message("Merging indicator labels for chart type: ", r$chart_type)
        d_before <- d
        d <- d |> sib_merge_ind_label(con = con)
        message("✓ Indicator labels merged")

        # Debug: Print processed data structure
        message("📊 PROCESSED DATA DEBUG:")
        if(nrow(d) > 0) {
          for(i in 1:min(3, nrow(d))) {
            row_data <- sapply(d[i,], function(x) if(is.null(x)) "NULL" else as.character(x))
            message("Row ", i, ": ", paste(names(d), "=", row_data, collapse = " | "))
          }
        }
      }

      old_data_rows <- if(is.null(r$main_data)) 0 else nrow(r$main_data)
      r$main_data <- d
      message("✓ r$main_data updated (was ", old_data_rows, " rows, now ", nrow(d), " rows)")

    }, error = function(e) {
      message("❌ ERROR fetching main data:")
      message("Error message: ", e$message)
      message("Error details: ", conditionMessage(e))
      r$main_data <- NULL
      message("✓ r$main_data set to NULL due to error")
    })

    message("🏁 DATA OBSERVER COMPLETED")
  })

  # Debug output for reactive values
  output$debug_reactive <- renderPrint({
    cat("=== Current Reactive Values ===\n")
    cat("sel_region:", r$sel_region, "\n")
    cat("sel_grupo_type:", r$sel_grupo_type, "\n")
    cat("sel_grupo:", r$sel_grupo, "\n")
    cat("sel_tematica:", r$sel_tematica, "\n")
    cat("sel_tipo:", r$sel_tipo, "\n")
    cat("chart_type:", r$chart_type, "\n")
    cat("indicador:", r$indicador, "\n")
    cat("breadcrumb:", r$breadcrumb, "\n")
    cat("amenazadas_categoria:", r$amenazadas_categoria, "\n")
    cat("cites_categoria:", r$cites_categoria, "\n")
    cat("especies_total_estimadas:", r$especies_total_estimadas, "\n")
    cat("show_subcategoria:", r$show_subcategoria, "\n")
    cat("show_especies_total_estimadas:", r$show_especies_total_estimadas, "\n")
    cat("available_charts:", paste(r$available_charts, collapse = ", "), "\n")
    cat("main_data rows:", if(is.null(r$main_data)) "NULL" else nrow(r$main_data), "\n")
    cat("species_data rows:", if(is.null(r$species_data)) "NULL" else nrow(r$species_data), "\n")
    cat("map_data rows:", if(is.null(r$map_data)) "NULL" else nrow(r$map_data), "\n")
  })

  # Close database connection when session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
}

shinyApp(ui, server)



