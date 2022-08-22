#'
#'
#' #' @export
#' list_species <- function(region,
#'                          grupo = NULL,
#'                          tematica = NULL,
#'                          validate_list = "warning"){
#'   # region <- "tolima"
#'   # grupo <- "algas"
#'
#'   especie <- sib_tables("especie")
#'   # Solo region
#'
#'   esp_reg <- sib_tables("especie_region") |>
#'     dplyr::filter(slug_region == region)
#'
#'   esp_tems <- sib_tables("especie_tematica") |>
#'     dplyr::filter(slug_region == region)
#'
#'   if(is.null(grupo) && is.null(tematica)){
#'     esps <- esp_reg |>
#'       arrange(desc(registros)) |>
#'       sib_merge_especie_label()
#'     sib_validate_list_especies(esps, region = region)
#'   }
#'
#'   if(!is.null(grupo) && is.null(tematica)){
#'
#'     # Returns the list of the region and grupo
#'     # grupo <- "algas"
#'     esp_grp <- sib_tables("especie_grupo") |>
#'       dplyr::filter(slug_grupo == grupo)
#'     # Check slug_especie is in group but also in region
#'     esps <- esp_grp |>
#'       filter(slug_especie %in% esp_reg$slug_especie) |>
#'       left_join(esp_reg, by = c("slug_especie")) |>
#'       arrange(desc(registros)) |>
#'       sib_merge_especie_label()
#'     sib_validate_list_especies(esps, region = region, grupo = grupo)
#'   }
#'
#'
#'
#'
#'
#'
#'
#'
#'   do_query_grupo <- !is.null(grupo)
#'   do_query_tematica <- !is.null(tematica)
#'
#'   #message("do_query_grupo ", do_query_grupo)
#'   #message("do_query_tematica ", do_query_tematica)
#'
#'
#'
#'
#'
#'
#'   if(do_query_grupo){
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'     reg_grp_tem <- sib_tables("region_grupo_tematica") |>
#'       dplyr::filter(slug_grupo == grupo) |>
#'       dplyr::filter(slug_region == region)
#'
#'
#'
#'     # Returns the list of the region and grupo
#'     # grupo <- "aves"
#'     esp_gr_bio <- sib_tables("region_grupo_tematica") |>
#'       dplyr::filter(slug_grupo == grupo)
#'     esp_reg_gr_bio <- esp_reg |>
#'       filter(slug_especie %in% esp_gr_bio$slug_especie)
#'
#'     if(do_query_tematica){
#'       # Returns the list of the region, tematica and grupo
#'       # tematica  <- "cites"
#'       esp_tem <- esp_tems |>
#'         dplyr::filter(grepl(tematica, slug_tematica)) |>
#'         distinct()
#'       especies <- esp_reg_gr_bio |>
#'         #filter(slug_especie %in% esp_tem$slug_especie)
#'         left_join(esp_tem) |>
#'         filter(!is.na(slug_tematica))
#'
#'     }else{
#'       especies <- esp_reg_gr_bio
#'
#'     }
#'   } else{
#'     if(do_query_tematica){
#'       # Returns the list of the region and tematica
#'       # tematica  <- "cites"
#'       esp_tem <- esp_tems |>
#'         dplyr::filter(grepl(tematica, slug_tematica)) |>
#'         left_join(esp_reg)
#'       especies <- esp_tem
#'
#'     }else{
#'       # Returns the list of the region only for all tematicas
#'       especies <- esp_reg
#'     }
#'   }
#'
#'   esp_with_name <- especies  |>
#'     left_join(especie, by = c("slug_especie" = "slug"))
#'   esp_with_name
#' }
#'
