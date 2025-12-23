
#' Check case values
#'
#' Valida que un valor sea válido para un parámetro dado (tipo, cobertura,
#' temática, indicador, grupo).
#'
#' @param param Nombre del parámetro a validar.
#' @param value Valor a validar.
#' @param con Conexión a la base de datos.
#' @param ... Argumentos adicionales (no usados).
#'
#' @return Invisible, lanza error si el valor no es válido.
#'
#' @export
check_cases_values <- function(param, value, con = NULL, ...) {
  # param <- "tematica"
  # value <- "cites_total"
  # value <- "exoticas_total"
  # value <- "exoticas_exoticas_riesgo_invasion"
  if(param %in% c("indicador","tipo", "cobertura", "tematica")){
    inds <- sibdata_indicadores(con = con)
    values <- inds |> select(one_of(param)) |> pull(1) |> unique()
    # if(param == "tematica"){
    #   subtematica <- inds |> select("subtematica") |> pull(1) |> unique()
    #   values <- c(values, subtematica)
    #   ### TODO Quick hack to fix exóticas
    #   if(value == "exoticas_riesgo_invasion")
    #     values <- c("exoticas_riesgo_invasion", values)
    # }

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


