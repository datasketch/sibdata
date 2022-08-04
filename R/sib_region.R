
#' @export
sib_region_general <- function(region){

  ## TODO validate varsnames
  vars <- c(
  "registros_region_total", "registros_continentales", "registros_marinos",
  "especies_region_total", "especies_continentales", "especies_marinas",
  "subtipo","label"
  )

  reg_data <- sib_calculate_region(region, vars)

  intro_tpl <- "A través del SiB Colombia se han publicado {registros_region_total} observaciones
  para el {subtipo} de {label}. Estos datos hacen referencia a un total de
  {especies_region_total} especies, de las cuales {especies_continentales} habitan el territorio
  al interior del continente y {especies_marinas} en el mar."

  reg_list <- transpose(reg_data)[[1]]
  reg_list$main_text <- glue::glue_data(reg_data, intro_tpl)

  reg_list

}

#' @export
sib_calculate_region <- function(region, vars = NULL){
  region_table <- sib_tables("region")
  reg <- region_table |>
    filter(slug == region)
  reg_tem <- sib_tables("region_tematica") |>
    filter(slug_region == region)
  reg <- reg |> left_join(reg_tem, by = c("slug" = "slug_region"))
  #lreg <- purrr::transpose(reg)[[1]]
  if(!is.null(vars)){
    if(!all(vars %in% names(lreg)))
      stop("All vars must be in data")
    lreg <- lreg[vars]
    reg <- reg |> select(any_of(vars))
  }
  reg
}


