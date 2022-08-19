
sibdata <- function(region,
                    tipo = NULL,
                    cobertura = NULL,
                    tematica = NULL,
                    grupo_biologico = NULL,
                    grupo_interes = NULL,
                    subregiones = NULL,
                    parent = NULL,
                    use_case = NULL){

  cases <- list(tipo = tipo,
                cobertura = cobertura,
                tematica = tematica,
                grupo_biologico = grupo_biologico,
                grupo_interes = grupo_interes,
                subregiones = subregiones,
                parent = parent)

  case <- check_cases(cases)

  if(case == "only_region"){
    d <- region_tematica(region) |>
      sib_merge_region_label()
  }

  if(case == "only_tipo"){
    d <- region_tematica(region) |>
      sib_merge_region_label()
    if(tipo == "todos"){
      d <- d
      # d <- d |>
      #   select(c("slug_region", "label"), starts_with(tipo))
    }else{
      d <- d |>
        select(c("slug_region", "label"), starts_with(paste0(tipo,"_region_total")))
    }
  }

  if(case == "only_cobertura"){
    d <- region_tematica(region) |>
      sib_merge_region_label()
    if(tipo == "total"){
      d <- d
      # d <- d |>
      #   select(c("slug_region", "label"), starts_with(tipo))
    }else{
      d <- d |>
        select(c("slug_region", "label"), starts_with(paste0(tipo,"_region_total")))
    }
  }

  if(case == "subregions"){
    d <- subregion_tematica(region)
  }
  if(case == "subregions_tipo"){
    d <- subregion_tematica(region) |>
      select(c("slug_region", "label"), starts_with(tipo))
  }

  return(d)

}

check_cases <- function(cases){

  cases_null <- purrr::map(cases, is.null)

  only_region <- all(unlist(cases_null))
  if(only_region) return("only_region")

  if(!cases_null$tipo){
    check_cases_values("tipo", cases$tipo)
    only_tipo <- only_param(cases, "tipo")
    if(only_tipo) return("only_tipo")
  }

  if(!cases_null$cobertura){
    check_cases_values("cobertura", cases$cobertura)
    only_cobertura <- only_param(cases, "cobertura")
    if(only_cobertura) return("only_cobertura")
  }


  subregions <- cases$subregiones %||% FALSE
  if(subregions){
    if(is.null(cases$tipo)){
      return("subregions")
    }else{
      return("subregions_tipo")
    }
  }

  parent <- cases$parent %||% FALSE
  if(parent){
  }

}


check_cases_values <- function(param, value){
  if(param == "tipo"){
    if(!value %in% c("registros", "especies", "todos")){
      stop('tipo must be one of: "registros", "especies", "todos"')
    }
  }
  if(param == "cobertura"){
    if(!value %in% c("continental", "marina", "total")){
      stop('cobertura must be one of: "continental", "marina", "total"')
    }
  }
}

only_param <- function(cases, param){
  cases_null <- purrr::map(cases, is.null)
  cases_not_param <- names(cases_null)[param != names(cases_null)]
  cases_null[[param]] == FALSE && all(unlist(cases_null[cases_not_param]))
}


sibdata_viz <- function(region, viz){

  if(viz == "map"){
    sibdata(region, subregiones = TRUE)
  }

}




