# Debug Module for SIB Data App
# This module provides configurable debug output

#' Debug Module UI
#' @param id Module ID
#' @param debug Boolean to control whether debug output is shown
#' @export
exp_debug_ui <- function(id, debug = FALSE) {
  ns <- NS(id)
  
  if (!debug) {
    return(NULL)  # Return NULL if debug is FALSE
  }
  
  tagList(
    h4("Debug - Reactive Values"),
    div(class = "debug-container",
        verbatimTextOutput(ns("debug_reactive"))
    ),
    hr()
  )
}

#' Debug Module Server
#' @param id Module ID
#' @param r Reactive values object
#' @param debug Boolean to control whether debug output is shown
#' @export
exp_debug_server <- function(id, r, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    # Only create debug output if debug is TRUE
    if (debug) {
      output$debug_reactive <- renderPrint({
        cat("=== Current Reactive Values ===\n")
        cat("sel_region:", r$sel_region, "\n")
        cat("sel_region_tipo:", r$sel_region_tipo, "\n")
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
    }
  })
} 