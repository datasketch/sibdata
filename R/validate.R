
#' Validate profile type
#'
#' Valida que un tipo de perfil sea válido.
#'
#' @param type Tipo de perfil a validar.
#'
#' @return Invisible, lanza error si el tipo no es válido.
#'
#' @export
sib_validate_profile_type <- function(type) {
  if (!type %in% sib_available_profile_types()) {
    stop(
      "Type must be one of: ",
      paste(sib_available_profile_types(), collapse = ", ")
    )
  }
}

#' Validate indicadores
#'
#' Valida que todos los indicadores proporcionados estén disponibles en la base
#' de datos.
#'
#' @param indicadores Vector con nombres de indicadores a validar.
#' @param con Conexión a la base de datos.
#'
#' @return Invisible, lanza error si algún indicador no es válido.
#'
#' @export
sib_validate_indicadores <- function(indicadores, con) {
  available_indicadores <- sibdata_ind_meta(con) |> pull(indicador)
  if (!all(indicadores %in% available_indicadores)) {
    message(dstools::which_not_in(indicadores, available_indicadores))
    stop("Not all indicadores in available_indicadores")
  }
}


#' Validate region
#'
#' Valida que una región esté disponible en la base de datos.
#'
#' @param region Slug de la región a validar.
#'
#' @return Invisible, lanza error si la región no es válida.
#'
#' @export
sib_validate_available_regions <- function(region) {
  available <- sib_available_regions()
  if (!region %in% available) {
    stop(
      "Region must be one of: ",
      paste(available, collapse = ", ")
    )
  }
}

#' Validate species list
#'
#' Valida que el número de especies en una lista coincida con el número
#' esperado según los parámetros.
#'
#' @param esps Data frame con lista de especies.
#' @param region Slug de la región.
#' @param grupo Slug del grupo biológico (opcional).
#' @param tematica Slug de la temática (opcional).
#' @param validate Tipo de validación: "warning" o "error" (default:
#'   "warning").
#'
#' @return Invisible, lanza warning o error según `validate` si no coinciden.
#'
#' @export
sib_validate_list_especies <- function(esps,
                                       region = NULL,
                                       grupo = NULL,
                                       tematica = NULL,
                                       validate = "warning") {
  n_esp <- sibdata(region, grupo = grupo, n_especies = TRUE, con = con)
  msg1 <- glue::glue(
    "Validando número especies: ",
    " region=", region,
    " grupo=", grupo, " \n"
  )
  message(msg1)
  if (nrow(esps) != n_esp) {
    msg <- waldo::compare(nrow(esps), n_esp, x_arg = "n list", y_arg = "sibdata")
    if (validate == "warning") warning(msg)
    if (validate == "error") stop(msg)
  }
}
