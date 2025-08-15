
map_icon <- function (sf = NULL, opts = NULL, save_path = NULL){

  default_opts <- geotable:::default_icon_opts()
  default_projections <- geotable:::default_projections()
  opts <- modifyList(default_opts, opts)



  union <- NULL

  g <- ggplot()

  fill <- opts$fill_color
  g <- g + geom_sf(data = sf, fill = fill, color = opts$main_border_color,
                   linewidth = opts$main_border_width)

  g <- g + geotable:::gg_theme_nothing(background = opts$background_color)
  if (!is.null(save_path)) {
    ggsave(filename = save_path, width = opts$save_width,
           height = opts$save_height)
  }
  g
}
