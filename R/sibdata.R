
#' @export
sibdata <- function(region = NULL,
                    tipo = NULL,
                    cobertura = NULL,
                    tematica = NULL,
                    indicador = NULL,
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
  if(!is.null(indicador)){
    check_cases_values("indicador", indicador, con = con)
  }
  d <- sibdata_wide(region = region,
                    tipo = tipo,
                    cobertura = cobertura,
                    tematica = tematica,
                    grupo = grupo,
                    indicador = indicador,
                    subregiones = subregiones,
                    with_parent = with_parent,
                    con = con)


  if(tidy){
    if(is.null(indicador)){
      d <- sibdata_tidify(d,
                          cobertura = cobertura,
                          tematica = tematica,
                          indicador = indicador,
                          all_indicators = all_indicators,
                          con = con)
    }else{
      # if("label" %in% names(d)){
      #   d <- d |>
      #     select(-label_region) |>
      #     rename(count = indicador)
      # }
    }
  }

  if(n_especies){
    n <- d |> filter(indicador == "especies_region_total") |> pull(count)
    return(n)
  }

  return(d)
}




sibdata_wide <- function(region = NULL,
                         tipo = NULL,
                         cobertura = NULL,
                         tematica = NULL,
                         grupo = NULL,
                         indicador = NULL,
                         subregiones = FALSE,
                         with_parent = FALSE,
                         con = NULL){
  d <- NULL
  if(is.null(region)) stop("Need a region")
  if(subregiones){
    if(!is.null(grupo)){
      d <- subregion_grupo(region, grupo, con = con)
    }else{
      d <- subregion_tematica(region, con = con)
    }
  } else if(with_parent){
    d <- with_parent_tematica(region, con = con)
  } else if (is.null(grupo) && !subregiones){
    d <- region_tematica(region, con = con)
  # } else if(is.null(tematica) && !subregiones){
  } else if(!subregiones){
    d <- region_grupo(region, grupo, con = con)
  }
  if(is.null(d)){
    stop("Cannot calculate d with the combinations of inputs provided:",
         paste("region=", region, "tipo=", tipo, "tematica=", tematica,
               "grupo=", grupo, "indicador=", indicador, "subregiones=", subregiones,
               "with_parent", with_parent)
    )
  }
  d <-  sib_merge_region_label(d, con = con)

  if(!is.null(indicador)){
    sel_inds <- indicador
  }else{
    sel_inds <- select_indicator(tipo = tipo,
                                 cobertura = cobertura,
                                 tematica = tematica,
                                 con = con)
    if(length(sel_inds) == 0){
      stop("No indicador found for tematica: ", tematica)
    }
  }

  # Si no hay GR definido
  d <- d |>
    select(contains(c("slug","label","grupo")), any_of(sel_inds)) |>
    collect()


  d
}


sibdata_tidify <- function(d,
                           tematica = NULL,
                           cobertura = NULL,
                           indicador = NULL,
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
    inds0 <- inds |>
      filter(tematica == cases$tematica)
    if(nrow(inds0) == 0){
      # Try with subtematica
      inds1 <- inds |>
        filter(subtematica == cases$tematica)
      # Try with categorias tematicas
      if(nrow(inds1) == 0){
        inds <- inds |>
          filter(categorias_tematicas == cases$tematica)
      }else{
        inds <- inds1
      }
    }else{
      inds <- inds0
    }
  }
  inds$indicador
}
