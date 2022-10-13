

#' @export
list_species <- function(region,
                         grupo = NULL,
                         tematica = NULL, with_labels = FALSE,
                         con = NULL){
  # region <- "tolima"
  # tematica <- "endemicas"
  # grupo <- "hongos"

  esp_reg <- sibdata_especie_region(con) |>
    filter(slug_region == region)

  especies <- esp_reg

  if(!is.null(tematica)){
    # especies <- sibdata_especie_tematica() |>
    #   filter(slug_region == region) |>
    #   filter(slug_tematica %like% paste0("%",tematica,"%")) |>
    #   left_join(esp_reg, by = c("slug_region", "slug_especie"))

    esp_tem <- sibdata_especie_tematica(con) |>
      filter(slug_region == region) |>
      filter(slug_tematica %like% paste0("%",tematica,"%"))

    especies <- especies |>
      left_join(esp_tem, by = c("slug_region", "slug_especie")) |>
      filter(!is.na(slug_tematica))
  }

  if(!is.null(grupo)){

    esp_gru <- sibdata_especie_grupo(con) |>
      filter(slug_grupo == grupo)

    especies <- especies |>
      left_join(esp_gru, by = "slug_especie") |>
      filter(!is.na(slug_grupo))
  }

  esp_with_name <- especies  |>
    left_join(sibdata_especie_meta(con), by = c("slug_especie" = "slug")) |>
    sib_merge_especie_label(con)

  if(with_labels){
    # esp_with_name$slug <- NULL
    # esp_with_name$species <- NULL

    esp_with_name <- esp_with_name |>
      select(-slug_especie, -species) |>
      rename("Especie" = "label", "Observaciones" = "registros",
             "Vernáculo" = "vernacular_name_es",
             "GBIF" = "url_gbif", "CBC" = "url_cbc",
             "Reino" = "kingdom", "Phylum" = "phylum", "Clase" = "class",
             "Orden" = "order", "Familia" = "family",
             "Genus" = "genus")
  }

  esp_with_name

}

