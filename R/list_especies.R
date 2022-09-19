

#' @export
list_species <- function(region,
                         grupo = NULL,
                         tematica = NULL){
  # region <- "tolima"

  especie <- sib_tables("especie")

  do_query_grupo <- !is.null(grupo)
  do_query_tematica <- !is.null(tematica)

  esp_reg <- sib_tables("especie_region") |>
    dplyr::filter(slug_region == region)
  esp_tems <- sib_tables("especie_tematica") |>
    dplyr::filter(slug_region == region)
  if(do_query_grupo){
    # Returns the list of the region and grupo
    # grupo <- "aves"
    esp_gr_bio <- sib_tables("especie_grupo") |>
      dplyr::filter(slug_grupo == grupo)
    esp_reg_gr_bio <- esp_reg |>
      filter(slug_especie %in% esp_gr_bio$slug_especie)

    if(do_query_tematica){
      # Returns the list of the region, tematica and grupo
      # tematica  <- "cites"
      esp_tem <- esp_tems |>
        dplyr::filter(grepl(tematica, slug_tematica)) |>
        distinct()
      especies <- esp_reg_gr_bio |>
        #filter(slug_especie %in% esp_tem$slug_especie)
        left_join(esp_tem) |>
        filter(!is.na(slug_tematica))

    }else{
      especies <- esp_reg_gr_bio

    }
  } else{
    if(do_query_tematica){
      # Returns the list of the region and tematica
      # tematica  <- "cites"
      esp_tem <- esp_tems |>
        dplyr::filter(grepl(tematica, slug_tematica)) |>
        left_join(esp_reg)
      especies <- esp_tem

    }else{
      # Returns the list of the region only for all tematicas
      especies <- esp_reg
    }
  }

  esp_with_name <- especies  |>
    left_join(especie, by = c("slug_especie" = "slug"))
  esp_with_name
}

