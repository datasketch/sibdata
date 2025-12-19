#' Access region table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `region`.
#'
#' @export
sibdata_region <- function(con) {
  dplyr::tbl(con, "region")
}

#' Access tematica table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `tematica`.
#'
#' @export
sibdata_tematica <- function(con) {
  dplyr::tbl(con, "tematica")
}

#' Access grupo table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `grupo`.
#'
#' @export
sibdata_grupo <- function(con) {
  dplyr::tbl(con, "grupo")
}

#' Access territorio table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `territorio`.
#'
#' @export
sibdata_territorio <- function(con) {
  dplyr::tbl(con, "territorio")
}

#' Access departamento table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `departamento`.
#'
#' @export
sibdata_departamento <- function(con) {
  dplyr::tbl(con, "departamento")
}

#' Access municipio table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `municipio`.
#'
#' @export
sibdata_municipio <- function(con) {
  dplyr::tbl(con, "municipio")
}

#' Access publicador table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `publicador`.
#'
#' @export
sibdata_publicador <- function(con) {
  dplyr::tbl(con, "publicador")
}

#' Access especie table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `especie`.
#'
#' @export
sibdata_especie <- function(con) {
  dplyr::tbl(con, "especie")
}

#' Access estimada table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `estimada`.
#'
#' @export
sibdata_estimada <- function(con) {
  dplyr::tbl(con, "estimada")
}

#' Access especie_meta table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `especie_meta`.
#'
#' @export
sibdata_especie_meta <- function(con) {
  dplyr::tbl(con, "especie_meta")
}

#' Access ind_meta table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `ind_meta`.
#'
#' @export
sibdata_ind_meta <- function(con) {
  dplyr::tbl(con, "ind_meta")
}

#' Access gallery_images table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `gallery_images`.
#'
#' @export
sibdata_gallery_images <- function(con) {
  dplyr::tbl(con, "gallery_images")
}

#' Access patrocinador table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `patrocinador`.
#'
#' @export
sibdata_patrocinador <- function(con) {
  dplyr::tbl(con, "patrocinador")
}

#' Access region_patrocinador table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `region_patrocinador`.
#'
#' @export
sibdata_region_patrocinador <- function(con) {
  dplyr::tbl(con, "region_patrocinador")
}

#' Access dato_relevante table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `dato_relevante`.
#'
#' @export
sibdata_dato_relevante <- function(con) {
  dplyr::tbl(con, "dato_relevante")
}

#' Access preg_frecuentes table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `preg_frecuentes`.
#'
#' @export
sibdata_preg_frecuentes <- function(con) {
  dplyr::tbl(con, "preg_frecuentes")
}

#' Access glosario table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `glosario`.
#'
#' @export
sibdata_glosario <- function(con) {
  dplyr::tbl(con, "glosario")
}

#' Access referencia_estimada table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `referencia_estimada`.
#'
#' @export
sibdata_referencia_estimada <- function(con) {
  dplyr::tbl(con, "referencia_estimada")
}

#' Access ranking table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `ranking`.
#'
#' @export
sibdata_ranking <- function(con) {
  dplyr::tbl(con, "ranking")
}

#' Access region_grupo table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `region_grupo`.
#'
#' @export
sibdata_region_grupo <- function(con) {
  dplyr::tbl(con, "region_grupo")
}

#' Access region_tematica table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `region_tematica`.
#'
#' @export
sibdata_region_tematica <- function(con) {
  dplyr::tbl(con, "region_tematica")
}

#' Access region_publicador table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `region_publicador`.
#'
#' @export
sibdata_region_publicador <- function(con) {
  dplyr::tbl(con, "region_publicador")
}

#' Access especie_tematica table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `especie_tematica`.
#'
#' @export
sibdata_especie_tematica <- function(con) {
  dplyr::tbl(con, "especie_tematica")
}

#' Access especie_grupo table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `especie_grupo`.
#'
#' @export
sibdata_especie_grupo <- function(con) {
  dplyr::tbl(con, "especie_grupo")
}

#' Access especie_region table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `especie_region`.
#'
#' @export
sibdata_especie_region <- function(con) {
  dplyr::tbl(con, "especie_region")
}

#' Access banner_images table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `banner_images`.
#'
#' @export
sibdata_banner_images <- function(con) {
  dplyr::tbl(con, "banner_images")
}

#' Access aporte_region_especial table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `aporte_region_especial`.
#'
#' @export
sibdata_aporte_region_especial <- function(con) {
  dplyr::tbl(con, "aporte_region_especial")
}

#' Access referencias_home table
#'
#' @param con Conexión a la base de datos.
#'
#' @return Objeto `tbl` de dplyr para la tabla `referencias_home`.
#'
#' @export
sibdata_referencias_home <- function(con) {
  dplyr::tbl(con, "referencias_home")
}
