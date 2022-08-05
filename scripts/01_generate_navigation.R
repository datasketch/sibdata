library(sibdata)


# generate_navigation
navigation_trees("region",
                 json_file = "static/data/nav_region.json")

navigation_trees("territorio",
                 json_file = "static/data/nav_territorio.json")


navigation_trees("grupo_biologico",
                 json_file = "static/data/nav_grupo_biologico.json")

navigation_trees("grupo_interes_conservacion",
                 json_file = "static/data/nav_grupo_interes_conservacion.json")

navigation_trees("tematica",
                 json_file = "static/data/nav_tematica.json")

# navigation_trees("territorio",
#                  json_file = "static/data/nav_tematica.json")


publicadores_to_json("static/data/publicador.json")


# Copy icons

copy_icons("static")


# Generate files for regions

av_regions <- sib_available_regions()

map(av_regions, function(region){
  # region <- "narino"
  subregs <- sib_available_subregions(region)
  parent <- sib_parent_region(region)

  reg_gr_bio <- sib_tables("region_grupo_biologico") |>
    dplyr::filter(slug_region == region) |>
    dplyr::mutate(slug = slug_grupo_biologico) |>
    dplyr::relocate(slug)
  reg_gr_int <- sib_tables("region_grupo_interes_conservacion") |>
    dplyr::filter(slug_region == region) |>
    dplyr::mutate(slug = slug_grupo_interes_conservacion) |>
    dplyr::relocate(slug)

  general_info <- sib_region_general(region)

  reg_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == region)
  subreg_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region %in% subregs)
  parent_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == parent)

  # Territorio

  d <- subreg_tematica
  munis_chart1 <- sib_chart_reg_municipios(d, "especies_region_total")
  path1 <- glue::glue("static/charts/{region}/region_municipios_1.html")
  htmlwidgets::saveWidget(munis_chart1, path1)
  munis_chart2 <- sib_chart_reg_municipios(d, "registros_region_total")
  path2 <- glue::glue("static/charts/{region}/region_municipios_2.html")
  htmlwidgets::saveWidget(munis_chart2, path2)

  territorio <- list(
    "municipios" = list(
      charts = list(
        list(title = "Especies por municipio", path = path1),
        list(title = "Observaciones por municipio", path = path2)
      )
    ),
    "reserva-forestal-la-planada" = list(
      list(title = "Próximamente tendrás acceso a la información de la reserva", path = NULL)
    ),
    "resguardo-indigena-pialapi-pueblo-viejo" = list(
      list(title = "Próximamente tendrás acceso a la información del resguardo", path = NULL)
    )
  )

  slides <- make_region_slides(region)

  l <- list(
    general_info = general_info,
    slides = slides,
    grupos_biologicos = reg_gr_bio,
    grupos_interes = reg_gr_int,
    tematica = reg_tematica,
    territorio = territorio
    )
  jsonlite::write_json(l, paste0("static/data/",region, ".json"),
                   auto_unbox = TRUE, pretty =TRUE)
})






