





region_tematica <- function(region){
  reg_tematica <- sibdata_region_tematica() |>
    dplyr::filter(slug_region == region)
  reg_tematica
}


region_grupo <- function(region, grupo){
  reg_grp_tematica <- sibdata_region_grupo() |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(slug_grupo == grupo)
  reg_grp_tematica
}




