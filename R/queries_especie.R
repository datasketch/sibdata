
especie_label <- function(){
  esp_label <- sib_tables("especie") |> select(slug, species)
  esp_label
}

especie_region <- function(region){
  sib_tables("especie_region") |>
    dplyr::filter(slug_region == region) |>
    left_join(esp_label, by = c("slug_especie" = "slug"))
}

especie_region_tematica <- function(region){
  esp_tem <- sib_tables("especie_tematica")
  esp_reg_tem <-  especie_region(region) |>
    dplyr::left_join(esp_tem, by = c("slug_region", "slug_especie")) |>
    dplyr::select(-registros) |>
    distinct()
}
