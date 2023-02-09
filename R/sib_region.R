
#' @export
sib_region_general <- function(region, con){

  ## TODO validate varsnames
  vars <- c(
    "especies_region_estimadas", "especies_region_total",
  "registros_region_total", "registros_continentales", "registros_marinos",
  "especies_continentales", "especies_marinas", "especies_continentales",
  "subtipo","label", "marino", "fecha_corte", "estimada_region_ref_id"
  )

  # region <- "reserva-forestal-la-planada"
  # region <-  "resguardo-indigena-pialapi-pueblo-viejo"


  reg_data <- sib_calculate_region(region, vars, con) |> collect()

  if(region == "reserva-forestal-la-planada"){
    reg_data$label <- "La Planada"
  }
  if(region == "resguardo-indigena-pialapi-pueblo-viejo"){
    reg_data$label <- "Pialapí Pueblo-Viejo"
  }

  reg_data$subtipo <- tolower(reg_data$subtipo)
  reg_data$marino <- as.logical(reg_data$marino)
  reg_data$fecha_corte <- as.character(reg_data$fecha_corte)
  if(is.na(reg_data$marino))
    reg_data$marino <- FALSE


  marino_text <- "."
  if(reg_data$marino){
    marino_text <- " de las cuales {especies_marinas} son especies marinas."
    marino_text <- "; de las cuales {especies_continentales} habitan al interior del
    continente y {especies_marinas} en el mar."
  }
  especies_marinas <-  makeup::makeup(reg_data$especies_marinas,"45.343,00")
  especies_continentales <-  makeup::makeup(reg_data$especies_continentales,"45.343,00")
  reg_data$marino_text <- glue::glue(marino_text)

  intro_tpl <- "A través del SiB Colombia se han publicado {registros_region_total_str} observaciones
  para el {subtipo} de {label}. Estos datos hacen referencia a un total de
  {especies_region_total_str} especies{marino_text}"

  if(region == "colombia"){
    intro_tpl <- "A {fecha_corte}, se han publicado {registros_region_total_str} observaciones a través
    del SiB Colombia. Estos datos respaldan la existencia de {especies_region_total_str} especies
    en el territorio nacional{marino_text}"
  }

  reg_data$especies_region_total_str <- makeup::makeup(reg_data$especies_region_total,"45.343,00")
  reg_data$registros_region_total_str <- makeup::makeup(reg_data$registros_region_total,"45.343,00")

  reg_list <- purrr::transpose(reg_data)[[1]]
  reg_list$main_text <- glue::glue_data(reg_data, intro_tpl)

  # Créditos y referencias

  estimada_ref_id <- reg_data$estimada_region_ref_id
  ref <- sibdata_referencia_estimada(con) |>
    filter(ref_id == estimada_ref_id) |>
      collect()
  reg_list$referencia <- ref$label
  reg_list$credito_foto <- glue::glue("{region}. Foto: Pepito Pérez. Creative Commons")
  reg_list

}

#' @export
sib_calculate_region <- function(region, vars = NULL, con = NULL){
  region_table <- sibdata_region(con) |> select(-marino)
  marinos <- sib_region_marino(con)
  region_table <- left_join(region_table, marinos,
                            by = "slug", copy = TRUE)

  reg <- region_table |>
    dplyr::filter(slug == region)
  reg_tem <- sibdata_region_tematica(con) |>
    dplyr::filter(slug_region == region)
  reg <- reg |> dplyr::left_join(reg_tem, by = c("slug" = "slug_region"))
  #lreg <- purrr::transpose(reg)[[1]]
  if(!is.null(vars)){
    if(!all(vars %in% colnames(reg)))
      stop("All vars must be in data")
    reg <- reg |> dplyr::select(any_of(vars))
  }
  reg
}

sib_region_marino <- function(con){
  deptos <- sibdata_departamento(con) |>
    select(slug, marino) |> collect()
  munis <- sibdata_municipio(con) |>
    select(slug, marino) |> collect()
  munis$slug[munis$slug == "colombia"] <- "colombia-hui"
  col <- tibble(slug = "colombia", marino = TRUE)
  bind_rows(col, deptos, munis)
}









