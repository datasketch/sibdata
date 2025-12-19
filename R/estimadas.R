
#' Get estimated species by grupo
#'
#' Obtiene estimaciones de especies amenazadas, CITES, endémicas para un grupo
#' biológico.
#'
#' @param grupo Slug del grupo biológico.
#' @param con Conexión a la base de datos.
#'
#' @return Lista con estimaciones de especies.
#'
#' @export
estimadas_grupo <- function(grupo, con) {

  d <- sibdata_estimada(con) |>
    filter(slug_grupo == grupo) |>
    select(slug_grupo,
           especies_amenazadas_nacional_total_estimadas,
           especies_amenazadas_global_total_estimadas,
           especies_cites_total_estimadas,
           especies_endemicas_estimadas) |>
    collect()
  as.list(d)
}



