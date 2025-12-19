
#' Get parent region tematica data
#'
#' Obtiene datos de temática de la región padre.
#'
#' @param region Slug de la región.
#'
#' @return Objeto `tbl` con datos de temática de la región padre.
#'
#' @keywords internal
parent_tematica <- function(region) {
  parent <- sib_parent_region(region, con = con)
  parent_tematica <- sibdata_region_tematica(con) |>
    dplyr::filter(slug_region == parent)
  parent_tematica
}

#' Get tematica data with parent region
#'
#' Obtiene datos de temática incluyendo la región y su región padre.
#'
#' @param region Slug de la región.
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` con datos de temática de la región y su padre.
#'
#' @keywords internal
with_parent_tematica <- function(region, con = NULL) {
  parent <- sib_parent_region(region, con)
  with_parent_tematica <- sibdata_region_tematica(con) |>
    dplyr::filter(slug_region %in% c(region, parent))
  with_parent_tematica
}

#' Get parent region
#'
#' Obtiene el slug de la región padre de una región dada, con casos especiales
#' para regiones naturales y áreas protegidas.
#'
#' @param region Slug de la región.
#' @param con Conexión a la base de datos.
#'
#' @return Slug de la región padre.
#'
#' @export
sib_parent_region <- function(region, con) {
  parent <- sibdata_region(con) |>
    dplyr::filter(slug == region) |>
    dplyr::pull(parent)
  parent <- parent[1]
  if (parent == "regiones-naturales") {
    parent <- "colombia"
  }
  if (region == "region-amazonia") {
    parent <- "colombia"
  }
  reserva_resguardo <- c(
    "reserva-forestal-la-planada",
    "resguardo-indigena-pialapi-pueblo-viejo"
  )
  if (region %in% reserva_resguardo) {
    parent <- "narino"
  }
  parent
}

