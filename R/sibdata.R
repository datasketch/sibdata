
#' @export
sibdata <- function(region,
                    tipo = NULL,
                    cobertura = NULL,
                    tematica = NULL,
                    grupo = NULL,
                    subregiones = FALSE,
                    with_parent = FALSE,
                    tidy = TRUE,
                    n_especies = FALSE,
                    all_indicators = FALSE){

  if(!is.null(tipo)) check_cases_values("tipo", tipo)
  if(!is.null(cobertura)) check_cases_values("cobertura", cobertura)
  if(!is.null(tematica)) check_cases_values("tematica", tematica)
  if(!is.null(grupo)) check_cases_values("grupo", grupo)

  d <- sibdata_wide(region,
                    tipo = tipo,
                    cobertura = cobertura,
                    tematica = tematica,
                    grupo = grupo,
                    subregiones = subregiones,
                    with_parent = with_parent)


  if(tidy){
    d <- sibdata_tidify(d, cobertura = cobertura,
                        tematica = tematica, all_indicators = all_indicators)
  }

  if(n_especies){
    n <- d |> filter(indicador == "especies_region_total") |> pull(count)
    return(n)
  }

  return(d)
}




sibdata_wide <- function(region,
                         tipo = NULL,
                         cobertura = NULL,
                         tematica = NULL,
                         grupo = NULL,
                         subregiones = FALSE,
                         with_parent = FALSE){
  if(subregiones){
    d <- subregion_tematica(region)
  } else if(with_parent){
    d <- with_parent_tematica(region)
  } else if (is.null(grupo)){
    d <- region_tematica(region)
  } else {
    d <- region_grupo(region, grupo)
  }
  d <-  d |> sib_merge_region_label()

  sel_inds <- select_indicator(tipo = tipo,
                               cobertura = cobertura,
                               tematica = tematica)

  # Si no hay GR definido
  d <- d |>
    select(contains(c("slug","label","grupo")), any_of(sel_inds)) |>
    collect()


  d
}


sibdata_tidify <- function(d,
                           tematica = NULL,
                           cobertura = NULL,
                           all_indicators = FALSE){



  inds <- sibdata_indicadores()

  not_tematica <- inds |> filter(is.na(tematica)) |> pull(indicador)
  not_cobertura <- inds |> filter(cobertura == "total") |> pull(indicador)
  is_cat_tematica <-  inds |>
    filter(!is.na(categorias_tematicas), categorias_tematicas != "total") |>
    pull(indicador)

  d2 <- d |>
    collect() |>
    pivot_longer(-contains(c("slug","grupo", "label")),
                 names_to = c("indicador"),
                 values_to = "count")

  if(all_indicators){
    return(d2)
  } else{

    d3 <- d2 |>
      #sib_merge_ind_label() |>
      select_non_single_cat_cols()

    if(is.null(tematica)){
      # Si no hay tematica definida retornar solo los totales
      d3 <- d3 |>
        filter(indicador %in% not_tematica)
    }else{
      d3 <- d3 |>
        filter(grepl(tematica, indicador) )
      if(grepl("amenazada|cites", tematica)){
        d3 <- d3 |>
          filter(indicador %in% is_cat_tematica) |>
          filter(grepl("_en|_cr|_vu|_i", indicador))
      }
    }
    if(is.null(cobertura)){
      d3 <- d3 |>
        filter(indicador %in% not_cobertura)
    }

  }

  d3

}


check_cases_values <- function(param, value){

  if(param %in% c("tipo", "cobertura", "tematica")){
    inds <- sibdata_indicadores()
    values <- inds |> select(one_of(param)) |> pull(1) |> unique()
    if(!value %in% values){
      stop("Value: ", value, ". ", param,' must be one of: ', paste0(values, collapse = ", "))
    }
  }
  if(param == "grupo"){
    values <- sib_available_grupos()
    if(!value %in% values){
      stop("Value: ",value, ". ", param,' must be one of: ', paste0(values, collapse = ", "))
    }
  }

}




select_indicator <- function(tipo = NULL,
                             cobertura = NULL,
                             tematica = NULL){

  cases <- list(tipo = tipo,
                cobertura = cobertura,
                tematica = tematica)

  inds <- sibdata_indicadores()

  if(!is.null(tipo)){
    inds <- inds |>
      filter(tipo == cases$tipo)
  }
  if(!is.null(cobertura)){
    inds <- inds |>
      filter(cobertura == cases$cobertura)
  }
  if(!is.null(tematica)){
    inds <- inds |>
      filter(tematica == cases$tematica)
  }
  inds$indicador
}
