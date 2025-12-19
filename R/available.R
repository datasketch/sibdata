
#' #' @export
#' sib_available_tables <- function(){
#'   sibdata::available_tables
#' }



#' Get available regions
#'
#' Obtiene lista de regiones disponibles, opcionalmente filtradas por subtipo o
#' departamento.
#'
#' @param subtipo Subtipo de región (ej: "País", "Departamento", "Municipio",
#'   "Especial").
#' @param departamento Slug del departamento para filtrar municipios (opcional).
#' @param con Conexión a la base de datos.
#'
#' @return Vector nombrado con slugs de regiones (nombres son las etiquetas).
#'
#' @export
sib_available_regions <- function(subtipo = NULL, departamento = NULL, con = con) {

  regs <- sibdata_region(con) |> collect()
  reg_gr <- sibdata_region_grupo(con) |>
    #select(slug_region) |>
    collect()
  sel_subtipo <- subtipo
  if(!is.null(subtipo)){
    if(subtipo == "Especial"){
      sel_subtipo <- c(
        "Territorios indígenas",
        "Reservas forestales protectoras",
        "Regiones naturales")
      regs <- regs |>
        filter(subtipo %in% sel_subtipo) |>
        filter(parent != "0")
    }else{
      regs <- regs |>
        filter(subtipo %in% sel_subtipo)

    }
  }
  #regs <- regs |> semi_join(reg_gr, by = c("slug" = "slug_region"))

  if(subtipo == "Municipio" & !is.null(departamento)){
    regs <- regs |>
      filter(parent == departamento)
  }

  av_regs <- regs |>
    distinct(slug) |>
    pull(slug)
  names(av_regs) <- regs |>
    distinct(slug, .keep_all = TRUE) |>
    pull(label)
  av_regs
}



#' Get available subregions
#'
#' Obtiene lista de subregiones de una región padre.
#'
#' @param region Slug de la región padre.
#' @param con Conexión a la base de datos.
#'
#' @return Vector con slugs de subregiones.
#'
#' @export
sib_available_subregions <- function(region, con) {
  region <- sibdata_region(con) |>
    dplyr::filter(parent == region)
  region |> dplyr::pull(slug)
}

#' Get available profile types
#'
#' Obtiene lista de tipos de perfiles disponibles.
#'
#' @return Vector con tipos de perfiles: "region", "territorio",
#'   "grupo_biologico", "grupo_interes", "specie", "tematica".
#'
#' @export
sib_available_profile_types <- function() {
  c(
    "region", "territorio", "grupo_biologico", "grupo_interes",
    "specie", "tematica"
  )
}

#' Get available grupos
#'
#' Obtiene lista de grupos biológicos o de interés de conservación disponibles.
#'
#' @param tipo Tipo de grupo: "biologico" o "interes" (opcional).
#' @param con Conexión a la base de datos.
#'
#' @return Vector nombrado con slugs de grupos (nombres son las etiquetas).
#'
#' @export
sib_available_grupos <- function(tipo = NULL, con) {
  grupo_tipo <- tipo
  grupo <- sibdata_grupo(con) |> collect()
  if (!is.null(tipo)) {
    grupo <- grupo |>
      filter(tipo == grupo_tipo)
  }
  av_grps <- grupo$slug
  names(av_grps) <- grupo$label
  av_grps
}

#' Get available tematicas
#'
#' Obtiene lista de temáticas disponibles para filtrar datos.
#'
#' @return Vector nombrado con slugs de temáticas (nombres son las etiquetas
#'   en español).
#'
#' @export
sib_available_tematicas <- function() {
  available_tematicas <- c(
    "Amenazadas Nacional" = "amenazadas_nacional",
    "Amenazadas Global" = "amenazadas_global",
    "Objeto de comercio (CITES)" = "cites",
    "Objeto de comercio (CITES I)" = "cites_i",
    "Objeto de comercio (CITES I_II)" = "cites_i_ii",
    "Objeto de comercio (CITES II)" = "cites_ii",
    "Objeto de comercio (CITES III)" = "cites_iii",
    "Endémicas" = "endemicas",
    "Migratorias" = "migratorias",
    "Exóticas Total" = "exoticas_total",
    "Exóticas" = "exoticas",
    "Invasoras" = "invasoras",
    "Exóticas riesgo invasión" = "exoticas_riesgo_invasion"
  )
  available_tematicas
}


