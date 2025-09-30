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

    # ALWAYS create debug output (it will just not be shown if debug=FALSE)
    # This ensures the reactive context is properly established
    output$debug_viz_reactive <- renderPrint({
      # Explicitly read ALL reactive values at the start to ensure proper invalidation
      # Read them in order of priority (most frequently changing first)
      main_data <- r$main_data
      sel_tipo <- r$sel_tipo
      indicador <- r$indicador
      sel_region <- r$sel_region
      sel_grupo <- r$sel_grupo
      tematica <- r$tematica
      chart_type <- r$chart_type
      inputs_ready <- r$inputs_ready

      if (!debug) return(NULL)

      cat("=== SIBDATA FUNCTION INPUTS ===\n")
      cat("region:", if(is.null(sel_region)) "NULL" else sel_region, "\n")
      cat("grupo:", if(is.null(sel_grupo)) "NULL" else sel_grupo, "\n")
      cat("tipo:", if(is.null(sel_tipo)) "NULL" else sel_tipo, "\n")
      cat("tematica:", if(is.null(tematica)) "NULL" else tematica, "\n")
      cat("indicador:", if(is.null(indicador)) "NULL" else indicador, "\n")
      cat("subregiones:", if(!is.null(chart_type) && chart_type == "map") "TRUE" else "FALSE", "\n")
      cat("with_parent: FALSE\n\n")

      cat("=== CURRENT CHART INFO ===\n")
      cat("chart_type:", if(is.null(chart_type)) "NULL" else chart_type, "\n")
      cat("inputs_ready:", if(is.null(inputs_ready)) "NULL" else as.character(inputs_ready), "\n\n")

      cat("=== MAIN DATA (r$main_data) GLIMPSE ===\n")
      if (!is.null(main_data)) {
        cat("Rows:", nrow(main_data), "\n")
        cat("Columns:", paste(names(main_data), collapse = ", "), "\n")
        dplyr::glimpse(main_data)
      } else {
        cat("r$main_data is NULL - no data available\n")
        cat("REASON: ")
        if (is.null(inputs_ready)) {
          cat("inputs_ready is NULL - data fetching hasn't started\n")
        } else if (!inputs_ready) {
          cat("inputs_ready is FALSE - waiting for inputs to be ready\n")
        } else {
          cat("inputs_ready is TRUE but data is still NULL - possible error in data fetching\n")
        }
      }
    })
  })
}
