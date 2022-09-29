library(sibdata)

#av_regions <- sib_available_regions(subtipo = c("Municipio"))
av_regions <- sib_available_regions(subtipo = c("Municipio"),
                                    departamento = "tolima")

av_regions <- c(
  "reserva-forestal-la-planada",
  "resguardo-indigena-pialapi-pueblo-viejo",
  av_regions
)

library(tictoc)

tic()

map(av_regions[1:5], function(region){
  message("\n...........",region)
  # region <- "ibague"
  # region <- "alpujarra"
  # region <- "alvarado"
  # region <- "reserva-forestal-la-planada"
  # region <-  "resguardo-indigena-pialapi-pueblo-viejo"

  nav_tematica <- navigation_trees("tematica")
  nav_grupo_biologico <- navigation_trees("grupo_biologico")
  nav_grupo_interes <- navigation_trees("grupo_interes")

  # No hay territorio
  nav_territorio <- list()

  general_info <- sib_region_general(region)

  # No hay galería
  gallery <- list()

  slides <- make_region_slides2(region)

  parent <- sib_parent_region(region)

  reg_gr_bio <- region_grupo_data(region, tipo = "biologico", verbose = TRUE)
  reg_gr_int <- region_grupo_data(region, tipo = "interes", verbose = TRUE)


  # Temáticas

  tem_list <- tematica_list(region)
  #tem_list <- NA




  ##
  patrocinadores <- sibdata_patrocinador()
  patrocinador <- sibdata_region_patrocinador() |>
    filter(slug_region == region)
  patrocinador <- patrocinador |>
    left_join(patrocinadores, by = c("slug_patrocinador" = "slug")) |>
    collect()

  publicadores <- sibdata_region_publicador() |>
    filter(slug_region == region) |>
    left_join(sibdata_publicador(), by = c("slug_publicador" = "slug")) |>
    select(slug_publicador, registros = registros.x, especies = especies.x,
           label, pais_publicacion,
           url_logo, url_socio) |>
    arrange(desc(registros)) |>
    collect()

  l <- list(
    nav_tematica = nav_tematica,
    nav_grupo_biologico = nav_grupo_biologico,
    nav_grupo_interes = nav_grupo_interes,
    nav_territorio = list(),

    general_info = general_info,

    gallery = list(),
    slides = slides,
    tematica = tem_list,
    grupos_biologicos = reg_gr_bio,
    grupos_interes = reg_gr_int,

    territorio = list(),

    patrocinador = patrocinador,
    publicadores = publicadores,
    municipios_lista = list()
  )
  jsonlite::write_json(l, paste0("static/data/",region, ".json"),
                       auto_unbox = TRUE, pretty =TRUE)


})


toc()




