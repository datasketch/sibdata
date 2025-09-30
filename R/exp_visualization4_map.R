# exp_visualization4_map.R
# Map visualization module for app4
# Isolates map rendering from main visualization module

#' Map Visualization Server Module
#'
#' Renders leaflet map based on reactive values
#' This is called as a nested module within exp_visualization4
#'
#' @param output The output object from parent module
#' @param r Reactive values object
#' @param con Database connection
#' @param debug Boolean to control console debug output
#' @export
exp_visualization4_map_server <- function(output, r, con, debug = FALSE) {
  # Map rendering - no moduleServer wrapper, just render directly
  output$map_viz <- leaflet::renderLeaflet({
      req(r$inputs_ready)
      req(r$main_data)
      req(r$chart_type == "map")

      if (debug) {
        message("🗺️ RENDERING MAP")
        message("- Data rows: ", nrow(r$main_data))
        message("- Region: ", r$sel_region)
        message("- Tipo: ", r$sel_tipo)
        message("- Indicador: ", r$indicador)
      }

      # Store map data for modal
      r$map_data <- r$main_data
      r$current_chart_data <- r$main_data

      # Render the map
      result <- choropleth_map(
        data = r$main_data,
        region = r$sel_region,
        tipo = r$sel_tipo,
        tematica = r$sel_tematica,
        indicador = r$indicador,
        grupo = r$sel_grupo,
        subregiones = TRUE,
        with_parent = FALSE,
        con = con,
        conmap = r$conmap
      )

      if (debug) message("✅ Map rendered successfully!")
      return(result)
  })
}