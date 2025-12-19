#' Get package resources path
#'
#' @param ... Componentes de la ruta del recurso.
#'
#' @return Ruta completa del recurso del paquete.
#'
#' @keywords internal
pkg_resources <- function(...) {
  system.file(..., package = "sibdata")
}

#' Paste string with dashes
#'
#' Crea una cadena con guiones para formatear texto jerárquico.
#'
#' @param str Cadena de texto.
#' @param times Número de niveles de indentación (guiones).
#'
#' @return Cadena formateada con guiones.
#'
#' @export
paste_dash <- function(str, times = 1) {
  paste(" ", paste0(rep("-", times - 1), collapse = ""), str)
}

#' Copy icons to directory
#'
#' Copia los iconos del paquete a un directorio especificado.
#'
#' @param path Ruta del directorio destino.
#'
#' @return Invisible, copia los archivos de iconos.
#'
#' @export
copy_icons <- function(path) {
  icons <- pkg_resources("icons")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  file.copy(icons, path, recursive = TRUE)
}

#' Select non-single category columns
#'
#' Elimina columnas que tienen un único valor (excepto indicador y count).
#'
#' @param x Data frame.
#'
#' @return Data frame sin columnas de un único valor.
#'
#' @keywords internal
select_non_single_cat_cols <- function(x) {
  ind_count <- x |> select(indicador, count)
  has_unique_vals <- function(xx) {
    if (is.numeric(xx)) return(FALSE)
    if (all(is.na(xx))) return(FALSE)
    length(unique(xx)) == 1
  }
  x |>
    select(-indicador, -count) |>
    select_if(~ !has_unique_vals(.)) |>
    bind_cols(ind_count)
}



#' Get system file path for sibdata package
#'
#' Obtiene la ruta de archivos del sistema del paquete sibdata.
#'
#' @param ... Componentes de la ruta del archivo.
#'
#' @return Ruta completa del archivo del sistema.
#'
#' @export
sys_file_sibdata <- function(...) {
  system.file(..., package = "sibdata")
}

#' Null or empty coalescing operator
#'
#' Operador que retorna el segundo argumento si el primero es NULL, NA o vacío.
#'
#' @param x Valor a verificar.
#' @param y Valor por defecto.
#'
#' @return `x` si no es NULL/NA/vacío, de lo contrario `y`.
#'
#' @keywords internal
`%||%` <- function(x, y) {
  suppressWarnings({
    if (is.empty(x)) {
      return(y)
    } else if (is.null(x) || is.na(x)) {
      return(y)
    } else if (class(x) == "character" && all(nchar(x) == 0)) {
      return(y)
    } else {
      x
    }
  })
}

#' Check if object is empty
#'
#' @param x Objeto a verificar.
#'
#' @return Logical indicando si el objeto está vacío.
#'
#' @keywords internal
is.empty <- function(x) {
  !as.logical(length(x))
}

#' Clean string
#'
#' Limpia una cadena de texto: convierte a minúsculas, elimina acentos y
#' puntuación.
#'
#' @param x Cadena de texto.
#'
#' @return Cadena limpia.
#'
#' @keywords internal
str_clean <- function(x) {
  x <- as.character(iconv(remove_accents(tolower(x)), to = "ASCII//TRANSLIT"))
  x <- trim_punct(x)
  stringr::str_squish(x)
}

#' Trim punctuation
#'
#' Elimina todos los caracteres de puntuación de una cadena.
#'
#' @param x Cadena de texto.
#'
#' @return Cadena sin puntuación.
#'
#' @keywords internal
trim_punct <- function(x) {
  gsub("[[:punct:]]", "", x)
}


#' Parse column name
#'
#' Convierte un índice numérico o nombre de columna en el nombre de columna
#' válido.
#'
#' @param d Data frame.
#' @param col Nombre o índice de columna (opcional, por defecto usa la primera
#'   columna).
#'
#' @return Nombre de la columna.
#'
#' @export
parse_col <- function(d, col = NULL) {
  if (is.null(col)) {
    col <- names(d)[1]
  } else {
    if (is.numeric(col)) col <- names(d)[col]
    if (!all(col %in% names(d))) {
      stop("Column not found in table")
    }
  }
  col
}




