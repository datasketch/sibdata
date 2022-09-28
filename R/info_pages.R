
save_info_page <- function(path){

  # Copy icons

  copy_icons("static")

  l <- info_pages()

  map2(l,names(l), function(page, nm){
    jsonlite::write_json(page,
                         file.path(path, paste0(nm, ".json")),
                         auto_unbox = TRUE, pretty = TRUE)
  })


}

info_pages <- function(){
  list(
    publicador = info_publicador(),
    preg_frecuentes = sibdata_preg_frecuentes() |> collect(),
    glosario = sibdata_glosario() |> collect(),
    tooltips = sibdata_tematica() |> select(slug, tooltip) |> collect()
  )

}

#' @export
info_publicador <- function(){

  which_regs <- c("boyaca", "narino", "tolima", "santander", "colombia")

  pub_col <- sibdata_region_publicador() |>
    collect() |>
    filter(slug_region == "colombia") |>
    select(slug = slug_publicador, registros, especies)
  pub_reg <- sibdata_region_publicador() |>
    collect() |>
    filter(slug_region %in% which_regs) |>
    sib_merge_region_label("slug_region") |>
    select(slug = slug_publicador, label_region) |>
    distinct() |>
    group_by(slug) |>
    summarise(region = str_flatten(label_region, collapse = ", "))

  pubs <- sibdata_publicador() |>
    collect() |>
    dplyr::distinct() |>
    left_join(pub_col, by = c("slug", "especies", "registros")) |>
    left_join(pub_reg, by = "slug")

  pubs
}


