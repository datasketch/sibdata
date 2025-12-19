#' Create map icon
#'
#' Genera un icono de mapa (visualización simplificada) a partir de un objeto
#' sf.
#'
#' @param sf Objeto `sf` con datos geoespaciales.
#' @param opts Lista con opciones de visualización (opcional).
#' @param save_path Ruta donde guardar el icono (opcional).
#'
#' @return Objeto ggplot con el icono del mapa.
#'
#' @keywords internal
map_icon <- function(sf = NULL, opts = NULL, save_path = NULL) {

  default_opts <- default_icon_opts()
  default_projections <- default_projections()
  opts <- modifyList(default_opts, opts)



  union <- NULL

  g <- ggplot()

  fill <- opts$fill_color
  g <- g + geom_sf(data = sf, fill = fill, color = opts$main_border_color,
                   linewidth = opts$main_border_width)

  g <- g + gg_theme_nothing(background = opts$background_color)
  if (!is.null(save_path)) {
    ggsave(filename = save_path, width = opts$save_width,
           height = opts$save_height)
  }
  g
}
