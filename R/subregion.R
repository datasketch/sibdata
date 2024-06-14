
subregion_tematica <- function(region, con){

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
  subreg_tematica
}


subregion_grupo <- function(region, grupo, con){

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

