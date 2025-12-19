
#' Create chart for regions/municipalities
#'
#' Crea un gráfico de barras para regiones o municipios.
#'
#' @param d Data frame con datos de regiones/municipios.
#' @param var Nombre de la variable a graficar (default: "especies_region_total").
#'
#' @return Objeto de gráfico Highcharts.
#'
#' @keywords internal
sib_chart_reg_municipios <- function(d, var = "especies_region_total") {

  x <- d |> dplyr::select(one_of(c("slug_region", var)))
  h <- hgch_bar_CatNum(x, hor_title = " ", ver_title = " ")
  h
}


#' Create waffle chart
#'
#' Crea un gráfico waffle (cuadrícula) para mostrar proporciones.
#'
#' @param d Data frame con datos.
#' @param path Ruta donde guardar el gráfico (opcional).
#'
#' @return Objeto ggplot con gráfico waffle.
#'
#' @export
sib_chart_waffle <- function(d, path) {

  x <- d$especies_region_total
  names(x) <- d$slug_region
  x[2] <- x[2] - x[1]
  #x <- rev(x)
  x <- round(x/sum(x)*100)

  gg <- waffle::waffle(x, colors = c("#6699FF", "#FFCC99"), row = 10,
               flip = TRUE) +
    theme(legend.position = "none")
  gg
}

#' Create gt table with bar chart
#'
#' Crea una tabla gt con gráfico de barras integrado.
#'
#' @param t Data frame con dos columnas (categoría y valor).
#' @param labels Vector con etiquetas para las columnas (opcional).
#' @param color Color de las barras (default: "#3e55ff").
#'
#' @return Objeto gt con tabla y gráfico de barras.
#'
#' @export
sib_chart_gt_table <- function(t, labels = NULL, color = "#3e55ff") {
  names(t) <- c("category", "n")
    gt <- gt::gt(t) |>
      #opt_table_font("Space Grotesk") |>
      gtExtras::gt_plt_bar(column = n, keep_column = TRUE,
                     width = 35, color = color) |>

      gt::tab_style(
        style = gt::cell_text(size = gt::px(12)),
        locations = list(gt::cells_body(), gt::cells_column_labels())
      )
    if(!is.null(labels)){
      labels <- paste0("**", labels, "**")
      gt <- gt |>
        gt::cols_label(category = gt::md(labels[1]), n = gt::md(labels[2]),
                   DUPE_COLUMN_PLT = "")
    }
    gt
}


#' Create gt table (version 2)
#'
#' Crea una tabla gt simple sin gráficos.
#'
#' @param t Data frame con dos columnas (especie y temática).
#' @param labels Vector con etiquetas para las columnas (opcional).
#'
#' @return Objeto gt con tabla.
#'
#' @export
sib_chart_gt_table2 <- function(t, labels = NULL) {
  names(t) <- c("especie", "tematica")
  gt <- gt::gt(t) |>
    #opt_table_font("Space Grotesk") |>
    gt::tab_style(
      style = gt::cell_text(size = gt::px(12)),
      locations = list(gt::cells_body(), gt::cells_column_labels())
    )
  gt
}


