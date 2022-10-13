

#' @export
sib_region_subgrupo <- function(region, grupo, con){
  #grupo <- "animales"

  subgrupos <- sibdata_grupo(con) |>
    filter(parent == grupo) |> pull(slug)

  reg <- sibdata_region_grupo(con) |>
    filter(slug_region == region) |>
    filter(slug_grupo %in% subgrupos) |>
    sib_merge_grupo_label("slug_grupo", con)
  reg
}
