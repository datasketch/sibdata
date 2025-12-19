
#' Get region labels
#'
#' Obtiene tabla con slugs y etiquetas de regiones.
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` con columnas `slug` y `label`.
#'
#' @keywords internal
sib_region_labels <- function(con) {
  sibdata_region(con) |>
    select(slug, label)
}


#' Merge region labels into data
#'
#' Agrega etiquetas de región a un data frame usando el slug de región.
#'
#' @param d Data frame con columna de slug de región.
#' @param slug Nombre de la columna con el slug de región (default:
#'   "slug_region").
#' @param label Nombre de la columna de etiqueta a crear (default:
#'   "label_region").
#' @param con Conexión a la base de datos.
#'
#' @return Data frame con columna de etiqueta agregada.
#'
#' @export
sib_merge_region_label <- function(d,
                                   slug = "slug_region",
                                   label = "label_region",
                                   con = con) {
  if(label %in% colnames(d)){
    d$label <- NULL
    warning("Overwritting existing label column: ", label,
            " Use the label param to rename the output label column.")
  }
  regs_label <- sibdata_region(con) |>
    select(slug_region = slug, label_region = label)

  if(slug == "slug"){
    by <- c("slug" = "slug_region")
  } else if(slug == "slug_region"){
    by <- "slug_region"
    if("label_region" %in% names(d)){
      d$label_region <- NULL
    }
  } else {
    stop('slug must be "slug" or "slug_region"')
  }

  if(!slug %in% colnames(d)){
    stop("Region slug column not found")
  }else{
    d2 <- d |>
      left_join(regs_label, by = by, copy = TRUE) |>
      relocate(label_region, .after = slug_region)
  }
  d2 |> distinct()
}


#' Merge grupo labels into data
#'
#' Agrega etiquetas de grupo biológico a un data frame usando el slug de grupo.
#'
#' @param d Data frame con columna de slug de grupo.
#' @param slug Nombre de la columna con el slug de grupo ("slug" o
#'   "slug_grupo").
#' @param con Conexión a la base de datos.
#'
#' @return Data frame con columna `label_grupo` agregada.
#'
#' @export
sib_merge_grupo_label <- function(d, slug, con) {
  grupo_labels <- sibdata_grupo(con) |>
    select(slug_grupo = slug, label_grupo = label)

  if(slug == "slug"){
    by <- c("slug" = "slug_grupo")
  } else if(slug == "slug_grupo"){
    by <- "slug_grupo"
  } else {
    stop('slug must be "slug" or "slug_grupo"')
  }

  if(!slug %in% colnames(d)){
    stop("Region slug column not found")
  }else{
    d2 <- d |>
      left_join(grupo_labels, by = by, copy = TRUE)
  }
  d2 |>
    relocate(label_grupo, .after = slug_grupo)

}



#' Merge indicator labels into data
#'
#' Agrega o reemplaza etiquetas de indicadores en un data frame o vector.
#'
#' @param d Data frame o vector de caracteres con nombres de indicadores.
#' @param replace Logical, reemplazar columna de indicador con etiqueta
#'   (default: `TRUE`).
#' @param con Conexión a la base de datos.
#'
#' @return Data frame o vector con etiquetas de indicadores.
#'
#' @export
sib_merge_ind_label <- function(d, replace = TRUE, con = con) {

  inds <- sibdata_indicadores(con = con) |>
    #filter(indicador %in% names(d)) |>
    select(indicador,label_ind = label)

  if("data.frame" %in% class(d)){
    if(!"indicador" %in% names(d)) return(d)
    dd <- left_join(d, inds, by = "indicador")
    if(replace){
      dd <- dd |>
        select(-indicador) |>
        select(indicador = label_ind, everything())
    }
    return(dd)
  }
  if("character" %in% class(d)){
    label <- dstools::match_replace(d, dic = inds, force = FALSE)
    return(label)
  }
  stop("Cannot merge labels in this object")
}


#' Merge especie labels into data
#'
#' Agrega etiquetas de especies (nombres científicos) a un data frame.
#'
#' @param x Data frame con columna `slug_especie`.
#' @param con Conexión a la base de datos.
#'
#' @return Data frame con columna `label` (nombre científico) agregada.
#'
#' @export
sib_merge_especie_label <- function(x, con) {
  especie <- sibdata_especie(con) |>
    mutate(label = species)
  x |>
    left_join(especie, by = c("slug_especie"="slug"), copy = TRUE) |>
    select(!contains("slug_region")) |>
    select(label, registros, everything())
}


#' Merge tematica labels into data
#'
#' Agrega etiquetas de temática a un data frame usando el slug de temática.
#'
#' @param d Data frame con columna `slug_tematica`.
#' @param con Conexión a la base de datos.
#'
#' @return Data frame con columna `tematica_label` agregada.
#'
#' @export
sib_merge_tematica_label <- function(d, con) {

  tematica_label <- sibdata_tematica(con) |>
    select(slug_tematica = slug, tematica_label = label) |>
    collect()

  d |>
    left_join(tematica_label, by = c("slug_tematica"), copy = TRUE)
}




