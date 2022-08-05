
#' @export
sib_available_tables <- function(){
  sibdata::available_tables
}



#' @export
sib_available_regions <- function(with_grupo = TRUE){
  if(with_grupo){
    av_reg_gr_bio <- sib_tables("region_grupo_biologico") |>
      dplyr::distinct(slug_region) |> dplyr::pull(slug_region)
    av_reg_gr_int <- sib_tables("region_grupo_interes_conservacion") |>
      dplyr::distinct(slug_region) |> dplyr::pull(slug_region)
    return(intersect(av_reg_gr_bio, av_reg_gr_int))
  }
  slugs <- sib_tables("region") |> dplyr::pull(slug)
  slugs
}

#' @export
sib_parent_region <- function(region){
  sib_tables("region") |>
    dplyr::filter(slug == region) |> dplyr::pull(parent)
}

#' @export
sib_available_subregions <- function(region){
  region <- sib_tables("region") |>
    dplyr::filter(parent == region)
  region |> dplyr::pull(slug)
}

#' @export
sib_available_profile_types <- function(){
  c("region", "territorio", "grupo_biologico", "grupo_interes_conservacion", "specie", "tematica")
}
