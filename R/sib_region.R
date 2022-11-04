
#' @export
sib_region_general <- function(region){

  ## TODO validate varsnames
  vars <- c(
    "especies_region_estimadas", "especies_region_total",
  "registros_region_total", "registros_continentales", "registros_marinos",
  "especies_continentales", "especies_marinas",
  "subtipo","label", "marino"
  )

  # region <- "reserva-forestal-la-planada"
  # region <-  "resguardo-indigena-pialapi-pueblo-viejo"


  reg_data <- sib_calculate_region(region, vars) |> collect()

  if(region == "reserva-forestal-la-planada"){
    reg_data$label <- "La Planada"
  }
  if(region == "resguardo-indigena-pialapi-pueblo-viejo"){
    reg_data$label <- "Pialapí Pueblo-Viejo"
  }

  reg_data$subtipo <- tolower(reg_data$subtipo)
  reg_data$marino <- as.logical(reg_data$marino)
  if(is.na(reg_data$marino))
    reg_data$marino <- FALSE

  marino_text <- "."
  if(reg_data$marino){
    marino_text <- " de las cuales {especies_marinas} son especies marinas."
  }
  especies_marinas <-  makeup::makeup(reg_data$especies_marinas,"45.343,00")
  reg_data$marino_text <- glue::glue(marino_text)

  intro_tpl <- "A través del SiB Colombia se han publicado {registros_region_total_str} observaciones
  para el {subtipo} de {label}. Estos datos hacen referencia a un total de
  {especies_region_total_str} especies{marino_text}"

  reg_data$especies_region_total_str <- makeup::makeup(reg_data$especies_region_total,"45.343,00")
  reg_data$registros_region_total_str <- makeup::makeup(reg_data$registros_region_total,"45.343,00")

  reg_list <- purrr::transpose(reg_data)[[1]]
  reg_list$main_text <- glue::glue_data(reg_data, intro_tpl)
  reg_list

}

#' @export
sib_calculate_region <- function(region, vars = NULL){
  region_table <- sibdata_region() |> select(-marino)
  marinos <- sib_region_marino()
  region_table <- left_join(region_table, marinos,
                            by = "slug", copy = TRUE)

  reg <- region_table |>
    dplyr::filter(slug == region)
  reg_tem <- sibdata_region_tematica() |>
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

sib_region_marino <- function(){
  deptos <- sibdata_departamento() |>
    select(slug, marino) |> collect()
  munis <- sibdata_municipio() |>
    select(slug, marino) |> collect()
  munis$slug[munis$slug == "colombia"] <- "colombia-hui"
  col <- tibble(slug = "colombia", marino = TRUE)
  bind_rows(col, deptos, munis)
}


