library(sibdata)

sib_available_tables()

# generate_navigation
# navigation_trees("region",
#                  json_file = "static/data/nav_region.json")
#
# navigation_trees("territorio",
#                  json_file = "static/data/nav_territorio.json")
#
#
# navigation_trees("grupo_biologico",
#                  json_file = "static/data/nav_grupo_biologico.json")
#
# navigation_trees("grupo_interes_conservacion",
#                  json_file = "static/data/nav_grupo_interes_conservacion.json")
#
# navigation_trees("tematica",
#                  json_file = "static/data/nav_tematica.json")

# navigation_trees("territorio",
#                  json_file = "static/data/nav_tematica.json")


publicadores_to_json("static/data/publicador.json")


preg_frecuentes <- sib_tables("preg_frecuentes")
jsonlite::write_json(preg_frecuentes, "static/data/preg_frecuentes.json",
                     auto_unbox = TRUE, pretty = TRUE)
glosario <- sib_tables("glosario")
jsonlite::write_json(glosario, "static/data/glosario.json",
                     auto_unbox = TRUE, pretty = TRUE)



tooltips <- sib_tables("tematica") |>
  select(slug, tooltip)
jsonlite::write_json(tooltips, "static/data/tooltips.json")

# Copy icons

copy_icons("static")





# Generate navigation files

# Generate files for regions

av_regions <- sib_available_regions()

library(tictoc)

tic()

map(av_regions, function(region){
  message(region)
  # region <- "boyaca"
  # region <- "narino"
  # region <- "tolima"
  # region <- "colombia"

  nav_tematica <- navigation_trees("tematica")
  nav_grupo_biologico <- navigation_trees("grupo_biologico")
  nav_grupo_interes <- navigation_trees("grupo_interes")
  nav_territorio <- navigation_trees("territorio", region = region)


  general_info <- sib_region_general(region)

  gallery <- make_gallery(region)

  slides <- make_region_slides(region)
  #slides <- list()

  reg_gr_bio <- region_gr_bio_data(region)


  reg_gr_int <- region_gr_int_data(region)


  # Temáticas

  tem_list <- tematica_list(region)
  #tem_list <- NA

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
    list(
      slug = "municipios",
      label = "Municipios",
      charts = list(
        list(title = "Especies por municipio", path = path1, layout = "title/chart"),
        list(title = "Observaciones por municipio", path = path2, layout = "title/chart")
      )
    ),
    list(
      slug = "areas-protegidas",
      label = "Áreas protegidas",
      title = "Próximamente tendrás acceso a la información de las áreas protegidas",
      charts = list()
    ),
    list(
      slug = "ecosistemas-estrategicos",
      label = "Ecosistemas estratégicos",
      title = "Próximamente tendrás acceso a la información de ecosistemas estratégicos",
      charts = list()
    )
  )

  ##
  patrocinadores <- sib_tables("patrocinador")
  patrocinador <- sib_tables("region_patrocinador") |>
    filter(slug_region == region)
  patrocinador <- patrocinador |>
    left_join(patrocinadores, by = c("slug_patrocinador" = "slug"))


  publicadores <- sib_tables("region_publicador") |>
    filter(slug_region == region) |>
    left_join(sib_tables("publicador"), by = c("slug_publicador" = "slug")) |>
    select(slug_publicador, registros = registros.x, especies = especies.x,
           label, pais_publicacion,
           url_logo, url_socio) |>
    arrange(desc(registros))



  municipios_lista <- subreg_tematica |>
    select(slug =slug_region, label)

  l <- list(
    general_info = general_info,
    nav_tematica = nav_tematica,
    nav_grupo_biologico = nav_grupo_biologico,
    nav_grupo_interes = nav_grupo_interes,
    nav_territorio = nav_territorio,

    gallery = gallery,
    slides = slides,
    tematica = tem_list,
    grupos_biologicos = reg_gr_bio,
    grupos_interes = reg_gr_int,
    territorio = territorio,
    patrocinador = patrocinador,
    publicadores = publicadores,
    municipios_lista = municipios_lista
  )
  jsonlite::write_json(l, paste0("static/data/",region, ".json"),
                       auto_unbox = TRUE, pretty =TRUE)



})


toc()




