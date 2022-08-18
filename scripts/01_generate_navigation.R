library(sibdata)

sib_available_tables()

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

tooltips <- sib_tables("tematica") |>
  select(slug, tooltip)
jsonlite::write_json(tooltips, "static/data/tooltips.json")




# Copy icons

copy_icons("static")


# Generate files for regions

av_regions <- sib_available_regions()

map(av_regions, function(region){
  # region <- "tolima"

  general_info <- sib_region_general(region)
  slides <- make_region_slides(region)
  reg_gr_bio <- region_gr_bio_data(region)
  reg_gr_int <- region_gr_int_data(region)




  tem_list <- tematica_list(region)

  # Territorio
  dir.create(glue::glue("static/charts/{region}"))

  subreg_tematica <- subregion_tematica(region)
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
    "areas-protegidas" = list(
      list(title = "Próximamente tendrás acceso a la información de las áreas protegidas", path = NULL)
    ),
    "reservas-naturales" = list(
      list(title = "Próximamente tendrás acceso a la información de las reservas naturales", path = NULL)
    )
  )

  ##
  patrocinadores <- sib_tables("patrocinador")
  patrocinador <- sib_tables("region_patrocinador") |>
    filter(slug_region == region)
  patrocinador <- patrocinador |>
    left_join(patrocinadores, by = c("slug_patrocinador" = "slug"))



  l <- list(
    general_info = general_info,
    slides = slides,
    grupos_biologicos = reg_gr_bio,
    grupos_interes = reg_gr_int,
    tematica = tematica_list,
    territorio = territorio,
    patrocinador = patrocinador
    )
  jsonlite::write_json(l, paste0("static/data/",region, ".json"),
                   auto_unbox = TRUE, pretty =TRUE)
})






