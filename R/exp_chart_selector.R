# exp_chart_selector.R
# Image-based chart selector module for SIB Data App
# Uses shinyinvoer::buttonImageInput like the original app

#' Chart Selector UI Module
#'
#' Creates an image-based chart selector using shinyinvoer::buttonImageInput
#' with dynamic chart availability based on data combinations
#'
#' @param id Module ID
#' @return UI elements for chart selector
#' @export
exp_chart_selector_ui <- function(id) {
  cat("📊 CHART SELECTOR UI CALLED with id:", id, "\n")
  ns <- NS(id)

  result <- tagList(
    # Chart type buttons (using buttonImageInput like original app)
    uiOutput(ns("chart_buttons"))
  )

  cat("📊 CHART SELECTOR UI returning:", length(result), "elements\n")
  return(result)
}

#' Chart Selector Server Module
#'
#' Handles chart type selection logic and dynamic chart availability
#' Uses shinyinvoer::buttonImageInput like the original app
#'
#' @param id Module ID
#' @param r Reactive values object
#' @param debug Boolean to control console debug output
#' @return Server logic for chart selector
#' @export
exp_chart_selector_server <- function(id, r, debug = FALSE) {
  cat("📊 CHART SELECTOR SERVER CALLED with id:", id, "\n")
  moduleServer(id, function(input, output, session) {
    cat("📊 CHART SELECTOR moduleServer CALLED\n")
    ns <- session$ns

    # Define all chart types (same as original app lines 360-361)
    all_charts <- c(
      "Mapa" = "map",
      "Torta" = "pie",
      "Dona" = "donut",
      "Treemap" = "treemap",
      "Barras" = "bar",
      "Tabla" = "table"
    )

    # Available charts are now computed centrally in app2.R
    # This module just uses r$available_charts

    # # Force trigger the renderUI by making it reactive to inputs_ready
    # observeEvent(r$inputs_ready, {
    #   if (debug) cat("📊 CHART SELECTOR observeEvent triggered, inputs_ready:", r$inputs_ready, "\n")
    # })

    # Force reactive dependencies with immediate initialization
    observe({
      cat("📊 CHART SELECTOR observe() triggered - checking reactive values\n")
      cat("📊 r$inputs_ready:", tryCatch(r$inputs_ready, error = function(e) "ERROR"), "\n") 
      cat("📊 r$available_charts length:", tryCatch(length(r$available_charts), error = function(e) "ERROR"), "\n")
      cat("📊 r$sel_tipo:", tryCatch(r$sel_tipo, error = function(e) "ERROR"), "\n")
      
      # Force invalidation on all reactive values we care about
      tryCatch({
        temp <- r$inputs_ready
        temp <- r$available_charts  
        temp <- r$sel_tipo
        cat("📊 All reactive dependencies accessed successfully\n")
      }, error = function(e) {
        cat("📊 ERROR accessing reactive dependencies:", e$message, "\n")
      })
    })
    
    # Additional immediate debugging
    cat("📊 CHART SELECTOR moduleServer - adding immediate observe priority\n")
    observe({
      cat("📊 HIGH PRIORITY observe() in chart selector triggered\n")
    }, priority = 1000)
    
    # Generate chart buttons UI using buttonImageInput (original app lines 366-374)
    output$chart_buttons <- renderUI({
      cat("📊 CHART SELECTOR moduleServer output$chart_buttons CALLED\n")

      # CRITICAL FIX: Remove req(r$inputs_ready) to avoid circular dependency
      # The container already handles inputs_ready dependency
      
      # Force reactive dependencies to trigger reactivity
      sel_tipo <- r$sel_tipo
      available_charts <- r$available_charts
      chart_type <- r$chart_type

      cat("📊 CHART BUTTONS renderUI called\n")
      cat("📊 sel_tipo:", sel_tipo, "\n")
      cat("📊 available_charts length:", if(is.null(available_charts)) "NULL" else length(available_charts), "\n")
      cat("📊 available_charts names:", if(is.null(available_charts)) "NULL" else paste(names(available_charts), collapse = ", "), "\n")
      cat("📊 chart_type:", chart_type, "\n")

      # # Don't use req() - let's see what happens without it
      # if (is.null(sel_tipo)) {
      #   if (debug) cat("📊 sel_tipo is NULL, returning NULL\n")
      #   return(NULL)
      # }
      #
      # if (is.null(available_charts) || length(available_charts) == 0) {
      #   if (debug) cat("📊 available_charts is NULL/empty, returning NULL\n")
      #   return(NULL)
      # }

      if (debug) cat("📊 Validation passed, creating buttons\n")

      # Get available charts from centralized reactive values
      av_charts <- r$available_charts

      # Set active chart (first available if current is not available)
      active_chart <- if(!is.null(r$chart_type) && r$chart_type %in% av_charts) {
        r$chart_type
      } else {
        av_charts[1]
      }

      # Update chart type in reactive values if it changed
      if(is.null(r$chart_type) || !r$chart_type %in% av_charts) {
        r$chart_type <- active_chart
        if (debug) message("Chart type automatically updated to: ", active_chart)
      }

      if (debug) {
        message("=== Chart Selector UI Update ===")
        message("Available charts: ", paste(names(av_charts), collapse = ", "))
        message("Active chart: ", active_chart)
        message("Disabled charts: ", paste(names(all_charts[!all_charts %in% av_charts]), collapse = ", "))
      }

      # Create buttonImageInput with grid layout for single row
      shinyinvoer::buttonImageInput(
        inputId = ns('chart_type'),
        images = all_charts,
        highlightColor = "#09A274",
        button_width = 28,
        path = 'www/viz_icons',
        active = active_chart,
        layout = "flex",
        disabled = all_charts[!all_charts %in% av_charts]
      )
    })

    # Handle chart selection - ALWAYS save to reactive values
    observeEvent(input$chart_type, {
      if(!is.null(input$chart_type)) {
        # Always save to reactive values, regardless of availability check
        # (availability is handled in the UI rendering)
        old_chart <- r$chart_type
        r$chart_type <- input$chart_type
        if (debug) message("Chart type saved to reactive values: ", old_chart, " -> ", input$chart_type)
      }
    })

    # Chart availability is now handled centrally in app2.R

  })
}

#' Get Chart Availability Logic
#'
#' Determines which charts are available based on current data combination
#' This replicates the logic from the original app (lines 340-355)
#'
#' @param tipo Data type ("registros" or "especies")
#' @param tematica Thematic category
#' @return Named vector of available charts
#' @export
get_available_charts <- function(tipo, tematica) {
  # Base chart types
  charts <- c("Mapa" = "map", "Torta" = "pie", "Dona" = "donut",
              "Treemap" = "treemap", "Barras" = "bar", "Tabla" = "table")

  # Restricted chart types
  map_table <- c("Mapa" = "map", "Tabla" = "table")
  map_table_bar <- c("Mapa" = "map", "Tabla" = "table", "Barras" = "bar")

  # Check if theme is special (amenazadas, cites, exoticas)
  is_special <- !is.null(tematica) &&
    (grepl("cites", tematica) || grepl("amenazadas", tematica) || grepl("exoticas", tematica))

  if(!is_special) {
    if(tipo == "registros") {
      return(map_table)
    }
    if(tipo == "especies") {
      return(map_table_bar)
    }
  }

  # For special themes, allow all charts
  return(charts)
}
