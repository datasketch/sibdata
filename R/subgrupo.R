

#' @export
sib_region_subgrupo <- function(region, grupo){
  #grupo <- "animales"

  subgrupos <- sibdata_grupo() |>
    filter(parent == grupo) |> pull(slug)

  reg <- sibdata_region_grupo() |>
    filter(slug_region == region) |>
    filter(slug_grupo %in% subgrupos) |>
    sib_merge_grupo_label("slug_grupo")
  reg
}
