
#' Get region subgrupo data
#'
#' Obtiene datos de subgrupos biológicos para una región y grupo padre.
#'
#' @param region Slug de la región.
#' @param grupo Slug del grupo biológico padre.
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` con datos de subgrupos.
#'
#' @export
sib_region_subgrupo <- function(region, grupo, con) {
  #grupo <- "animales"

  subgrupos <- sibdata_grupo(con) |>
    filter(parent == grupo) |> pull(slug)

  reg <- sibdata_region_grupo(con) |>
    filter(slug_region == region) |>
    filter(slug_grupo %in% subgrupos) |>
    sib_merge_grupo_label("slug_grupo", con)
  reg
}
