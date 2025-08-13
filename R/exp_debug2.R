# Debug Module 2 for SIB Data App - Visualization Debug
# This module provides debug output specifically for visualization reactive values

#' Debug Module 2 UI - Visualization Debug
#' @param id Module ID
#' @export
exp_debug2_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("debug"))

}

#' Debug Module 2 Server - Visualization Debug
#' @param id Module ID
#' @param r Reactive values object
#' @param debug Boolean to control whether debug output is shown
#' @export
exp_debug2_server <- function(id, r, debug = FALSE) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$debug <- renderUI({
      if(!debug) return(NULL)

      tagList(
        h5("Debug - Información de Visualización", style = "color: #666; margin-top: 20px;"),
        div(class = "debug-container",
            verbatimTextOutput(ns("debug_viz_reactive"))
        )
      )

    })

    # Only create debug output if debug is TRUE
    if (debug) {
      output$debug_viz_reactive <- renderPrint({

        cat("=== SIBDATA FUNCTION INPUTS ===\n")
        cat("region:", if(is.null(r$sel_region)) "NULL" else r$sel_region, "\n")
        cat("grupo:", if(is.null(r$sel_grupo)) "NULL" else r$sel_grupo, "\n")
        cat("tipo:", if(is.null(r$sel_tipo)) "NULL" else r$sel_tipo, "\n")
        cat("tematica:", if(is.null(r$tematica)) "NULL" else r$tematica, "\n")
        cat("indicador:", if(is.null(r$indicador)) "NULL" else r$indicador, "\n")
        cat("subregiones:", if(!is.null(r$chart_type) && r$chart_type == "map") "TRUE" else "FALSE", "\n")
        cat("with_parent: FALSE\n\n")

        cat("=== CURRENT CHART INFO ===\n")
        cat("chart_type:", if(is.null(r$chart_type)) "NULL" else r$chart_type, "\n")
        cat("inputs_ready:", if(is.null(r$inputs_ready)) "NULL" else as.character(r$inputs_ready), "\n\n")

        cat("=== MAIN DATA (r$main_data) GLIMPSE ===\n")
        if (!is.null(r$main_data)) {
          dplyr::glimpse(r$main_data)
        } else {
          cat("r$main_data is NULL - no data available\n")
          cat("REASON: ")
          if (is.null(r$inputs_ready)) {
            cat("inputs_ready is NULL - data fetching hasn't started\n")
          } else if (!r$inputs_ready) {
            cat("inputs_ready is FALSE - waiting for inputs to be ready\n")
          } else {
            cat("inputs_ready is TRUE but data is still NULL - possible error in data fetching\n")
          }
        }
      })
    }
  })
}
