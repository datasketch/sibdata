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
  ns <- NS(id)

  tagList(
    # Chart type buttons (using buttonImageInput like original app)
    uiOutput(ns("chart_buttons"))
  )
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
  moduleServer(id, function(input, output, session) {
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

    # Generate chart buttons UI using buttonImageInput (original app lines 366-374)
    output$chart_buttons <- renderUI({
      req(r$sel_tipo)
      req(r$available_charts)

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
