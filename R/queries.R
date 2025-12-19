




#' Get region tematica data
#'
#' Obtiene datos de temática para una región.
#'
#' @param region Slug de la región.
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` con datos de temática de la región.
#'
#' @keywords internal
region_tematica <- function(region, con) {
  reg_tematica <- sibdata_region_tematica(con) |>
    dplyr::filter(slug_region == region)
  reg_tematica
}

#' Get region grupo data
#'
#' Obtiene datos de grupo biológico para una región.
#'
#' @param region Slug de la región.
#' @param grupo Slug del grupo biológico.
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` con datos de grupo de la región.
#'
#' @keywords internal
region_grupo <- function(region, grupo, con) {
  reg_grp_tematica <- sibdata_region_grupo(con) |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(slug_grupo == grupo)
  reg_grp_tematica
}




