#' Get subregion tematica data
#'
#' Obtiene datos de temática para todas las subregiones de una región padre.
#'
#' @param region Slug de la región padre.
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` con datos de temática de subregiones.
#'
#' @export
subregion_tematica <- function(region, con) {

  regs <- sibdata_region(con) |>
    select(slug_region = slug, label)

  subregs <- sib_available_subregions(region, con)
  if(region == "bogota-dc"){
    subregs <- "bogota-dc"
  }

  if(region == "colombia"){
    subregs <- c(subregs, "bogota-dc")
  }

  subreg_tematica <- sibdata_region_tematica(con) |>
    dplyr::filter(slug_region %in% subregs) |>
    dplyr::left_join(regs, by = "slug_region") |>
    dplyr::select(-fecha_corte) |>
    dplyr::relocate(slug_region, label, everything())
  subreg_tematica |> distinct()
}

#' Get subregion grupo data
#'
#' Obtiene datos de grupo biológico para todas las subregiones de una región
#' padre.
#'
#' @param region Slug de la región padre.
#' @param grupo Slug del grupo biológico.
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` con datos de grupo de subregiones.
#'
#' @export
subregion_grupo <- function(region, grupo, con) {

  grp <- grupo
  regs <- sibdata_region(con) |>
    select(slug_region = slug, label)

  subregs <- sib_available_subregions(region, con)
  if(region == "bogota-dc"){
    subregs <- "bogota-dc"
  }

  if(region == "colombia"){
    subregs <- c(subregs, "bogota-dc")
  }

  subreg_grupo <- sibdata_region_grupo(con) |>
    dplyr::filter(slug_grupo == grp) |>
    dplyr::filter(slug_region %in% subregs) |>
    dplyr::left_join(regs, by = "slug_region") |>
    dplyr::relocate(slug_region, label, everything())
  subreg_grupo
}

