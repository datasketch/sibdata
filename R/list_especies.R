

#' @export
list_species <- function(region,
                         grupo = NULL,
                         tematica = NULL){
  # region <- "tolima"
  # tematica <- "endemicas"
  # grupo <- "hongos"

  esp_reg <- sibdata_especie_region() |>
    filter(slug_region == region)

  especies <- esp_reg

  if(!is.null(tematica)){
    # especies <- sibdata_especie_tematica() |>
    #   filter(slug_region == region) |>
    #   filter(slug_tematica %like% paste0("%",tematica,"%")) |>
    #   left_join(esp_reg, by = c("slug_region", "slug_especie"))

    esp_tem <- sibdata_especie_tematica() |>
      filter(slug_region == region) |>
      filter(slug_tematica %like% paste0("%",tematica,"%"))

    especies <- especies |>
      left_join(esp_tem, by = c("slug_region", "slug_especie")) |>
      filter(!is.na(slug_tematica))
  }

  if(!is.null(grupo)){

    esp_gru <- sibdata_especie_grupo() |>
      filter(slug_grupo == grupo)

    especies <- especies |>
      left_join(esp_gru, by = "slug_especie") |>
      filter(!is.na(slug_grupo))
  }

  esp_with_name <- especies  |>
    left_join(sibdata_especie_meta(), by = c("slug_especie" = "slug")) |>
    sib_merge_especie_label()

  esp_with_name

}

