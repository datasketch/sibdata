
save_info_page <- function(path, con){

  # Copy icons

  copy_icons("static")

  l <- info_pages(con)

  map2(l,names(l), function(page, nm){
    jsonlite::write_json(page,
                         file.path(path, paste0(nm, ".json")),
                         auto_unbox = TRUE, pretty = TRUE)
  })


}

info_pages <- function(con){
  list(
    publicador = info_publicador(con),
    preg_frecuentes = sibdata_preg_frecuentes(con) |> collect(),
    glosario = sibdata_glosario(con) |> collect(),
    tooltips = sibdata_tematica(con) |> select(slug, tooltip) |> collect()
  )

}

#' @export
info_publicador <- function(con){

  which_regs <- c("boyaca", "narino", "tolima", "santander", "colombia")
  which_regs <- sibdata_region(con) |> collect() |>
    filter(parent %in% which_regs) |>
    pull(slug)

  pub_col <- sibdata_region_publicador(con) |>
    collect() |>
    filter(slug_region == "colombia") |>
    select(slug = slug_publicador, registros, especies)
  pub_reg <- sibdata_region_publicador(con) |>
    collect() |>
    filter(slug_region %in% which_regs) |>
    sib_merge_region_label("slug_region", con = con) |>
    select(slug = slug_publicador, label_region) |>
    distinct() |>
    group_by(slug) |>
    summarise(region = str_flatten(label_region, collapse = ", "))

  pubs <- sibdata_publicador(con) |>
    collect() |>
    dplyr::distinct() |>
    left_join(pub_col, by = c("slug", "especies", "registros")) |>
    left_join(pub_reg, by = "slug")

  pubs
}


