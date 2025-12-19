
#' Get indicators metadata
#'
#' Obtiene metadatos de indicadores con subtemática calculada.
#'
#' @param con Conexión a la base de datos.
#'
#' @return Data frame con metadatos de indicadores incluyendo columna
#'   `subtematica`.
#'
#' @export
sibdata_indicadores <- function(con) {
  sibdata_ind_meta(con) |>
    collect() |>
    mutate(subtematica = paste(tematica, categorias_tematicas, sep = "_"))
}
