

parent_tematica <- function(region){
  parent <- sib_parent_region(region)
  parent_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == parent)
  parent_tematica
}

with_parent_tematica <- function(region){
  parent <- sib_parent_region(region)
  with_parent_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region %in% c(region, parent))
  with_parent_tematica
}

#' @export
sib_parent_region <- function(region){
  sib_tables("region") |>
    dplyr::filter(slug == region) |> dplyr::pull(parent)
}

