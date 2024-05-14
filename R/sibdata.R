
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
                    all_indicators = FALSE,
                    con = con){

  if(!is.null(tipo)) check_cases_values("tipo", tipo, con = con)
  if(!is.null(cobertura)) check_cases_values("cobertura", cobertura, con = con)
  if(!is.null(tematica)) check_cases_values("tematica", tematica, con = con)
  if(!is.null(grupo)) check_cases_values("grupo", grupo, con = con)

  d <- sibdata_wide(region,
                    tipo = tipo,
                    cobertura = cobertura,
                    tematica = tematica,
                    grupo = grupo,
                    subregiones = subregiones,
                    with_parent = with_parent,
                    con = con)


  if(tidy){
    d <- sibdata_tidify(d,
                        cobertura = cobertura,
                        tematica = tematica, all_indicators = all_indicators,
                        con = con)
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
                         with_parent = FALSE,
                         con = NULL){
  if(subregiones){
    d <- subregion_tematica(region, con = con)
  } else if(with_parent){
    d <- with_parent_tematica(region, con = con)
  } else if (is.null(grupo)){
    d <- region_tematica(region, con = con)
  } else {
    d <- region_grupo(region, grupo, con = con)
  }
  d <-  d |> sib_merge_region_label(con = con)

  sel_inds <- select_indicator(tipo = tipo,
                               cobertura = cobertura,
                               tematica = tematica,
                               con = con)

  # Si no hay GR definido
  d <- d |>
    select(contains(c("slug","label","grupo")), any_of(sel_inds)) |>
    collect()


  d
}


sibdata_tidify <- function(d,
                           tematica = NULL,
                           cobertura = NULL,
                           all_indicators = FALSE,
                           con = NULL){



  inds <- sibdata_indicadores(con)

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


check_cases_values <- function(param, value, con = NULL){

  if(param %in% c("tipo", "cobertura", "tematica")){
    inds <- sibdata_indicadores(con = con)
    values <- inds |> select(one_of(param)) |> pull(1) |> unique()
    if(!value %in% values){
      stop("Value: ", value, ". ", param,' must be one of: ', paste0(values, collapse = ", "))
    }
  }
  if(param == "grupo"){
    values <- sib_available_grupos(con = con)
    if(!value %in% values){
      stop("Value: ",value, ". ", param,' must be one of: ', paste0(values, collapse = ", "))
    }
  }

}




select_indicator <- function(tipo = NULL,
                             cobertura = NULL,
                             tematica = NULL,
                             con = NULL){

  cases <- list(tipo = tipo,
                cobertura = cobertura,
                tematica = tematica)

  inds <- sibdata_indicadores(con)

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
