# Debug Module for SIB Data App
# This module provides configurable debug output

#' Debug Module UI
#' @param id Module ID
#' @param debug Boolean to control whether debug output is shown
#' @export
exp_debug_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("debug"))

}

#' Debug Module Server
#' @param id Module ID
#' @param r Reactive values object
#' @param debug Boolean to control whether debug output is shown
#' @export
exp_debug_server <- function(id, r, debug = FALSE) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # message("DEBUG value: ", debug)
    output$debug <- renderUI({
      if(!debug) return(NULL)

      tagList(
        h4("Debug - Reactive Values"),
        div(class = "debug-container",
            verbatimTextOutput(ns("debug_reactive"))
        ),
        hr()
      )

    })

    # Only create debug output if debug is TRUE
    if (debug) {
      output$debug_reactive <- renderPrint({
        cat("=== Current Reactive Values ===\n")
        cat("inputs_ready:", r$inputs_ready, "\n")
        cat("sel_region:", r$sel_region, "\n")
        cat("sel_region_tipo:", r$sel_region_tipo, "\n")
        cat("sel_grupo_tipo:", r$sel_grupo_tipo, "\n")
        cat("sel_grupo:", r$sel_grupo, "\n")
        cat("sel_tematica:", r$sel_tematica, "\n")
        cat("sel_subtematica:", r$sel_subtematica, "\n")
        cat("sel_tipo:", r$sel_tipo, "\n")
        cat("tematica:", r$tematica, "\n")
        cat("indicador:", r$indicador, "\n")
        cat("chart_type:", r$chart_type, "\n")
        cat("breadcrumb:", r$breadcrumb, "\n")
        cat("is_special_region:", r$is_special_region, "\n")
        cat("has_subtematica:", r$has_subtematica, "\n")
        cat("amenazadas_categoria:", r$amenazadas_categoria, "\n")
        cat("cites_categoria:", r$cites_categoria, "\n")
        cat("exotica_categoria:", r$exotica_categoria, "\n")
        # cat("especies_total_estimadas:", r$especies_total_estimadas, "\n")
        # cat("show_subcategoria:", r$show_subcategoria, "\n")
        # cat("show_especies_total_estimadas:", r$show_especies_total_estimadas, "\n")
        cat("available_charts:", paste(r$available_charts, collapse = ", "), "\n")
        cat("--- DATA STORAGE ---\n")
        cat("main_data rows:", if(is.null(r$main_data)) "NULL" else nrow(r$main_data), "\n")
        cat("species_data rows:", if(is.null(r$species_data)) "NULL" else nrow(r$species_data), "\n")
        cat("map_data rows:", if(is.null(r$map_data)) "NULL" else nrow(r$map_data), "\n")
        cat("table_data rows:", if(is.null(r$table_data)) "NULL" else nrow(r$table_data), "\n")
        cat("chart_data rows:", if(is.null(r$chart_data)) "NULL" else nrow(r$chart_data), "\n")
        cat("current_chart_data rows:", if(is.null(r$current_chart_data)) "NULL" else nrow(r$current_chart_data), "\n")
        if (!is.null(r$current_chart_data) && !is.null(r$chart_type)) {
          cat("current_chart_data for:", r$chart_type, "\n")
          cat("current_chart_data cols:", paste(names(r$current_chart_data), collapse = ", "), "\n")
        }

        # Add main_data glimpse using dplyr::glimpse()
        cat("--- MAIN DATA GLIMPSE ---\n")
        if (!is.null(r$main_data)) {
          dplyr::glimpse(r$main_data)
        } else {
          cat("main_data is NULL\n")
        }
      })
    }

  })
}
