





region_tematica <- function(region, con){
  reg_tematica <- sibdata_region_tematica(con) |>
    dplyr::filter(slug_region == region)
  reg_tematica
}


region_grupo <- function(region, grupo, con){
  reg_grp_tematica <- sibdata_region_grupo(con) |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(slug_grupo == grupo)
  reg_grp_tematica
}




