library(sibdata)
library(lfltmagic)
devtools::load_all()


here::dr_here()
#here::set_here("./..")
setwd("../")
here::dr_here()


con <- DBI::dbConnect(duckdb::duckdb(), sys_file("db/sibdata.duckdb"),
                      read_only = TRUE)

#av_regions <- sib_available_regions(subtipo = c("Municipio"))
av_regions2 <- sib_available_regions(subtipo = c("Municipio"),
                                    departamento = "tolima", con = con)

av_regions1 <- sib_available_regions(subtipo = c("Municipio"),
                                    departamento = "narino", con = con)

av_regions3 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "boyaca", con = con)
av_regions4 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "santander", con = con)

av_regions <- c(
  # "reserva-forestal-la-planada",
  # "resguardo-indigena-pialapi-pueblo-viejo",
  # av_regions1,
  av_regions2
  # av_regions3,
  # av_regions4
)

library(tictoc)

tic()

map(av_regions[35], function(region){
  message("\n...........",region)
  # region <- "ibague"
  # region <- "alpujarra"
  # region <- "alvarado"
  # region <- "reserva-forestal-la-planada"
  # region <-  "resguardo-indigena-pialapi-pueblo-viejo"
  # region <- "purificacion"

  nav_tematica <- navigation_trees("tematica", con = con)
  nav_grupo_biologico <- navigation_trees("grupo_biologico", con = con)
  nav_grupo_interes <- navigation_trees("grupo_interes", con = con)

  # No hay territorio
  nav_territorio <- list()

  general_info <- sib_region_general(region, con = con)

  # No hay galería
  gallery <- list()

  slides <- make_region_slides2(region, con = con)

  parent <- sib_parent_region(region, con = con)

  general_info$parent <- parent
  parent_depto <- parent
  general_info$parent_label <- sibdata_region(con) |>
    collect() |>
    filter(slug == parent_depto) |> pull(label)



  if(region == "reserva-forestal-la-planada"){
    parent <- "narino"
  }
  if(region == "resguardo-indigena-pialapi-pueblo-viejo"){
    parent <- "narino"
  }

  reg_gr_bio <- list()
  reg_gr_int <- list()

  if(parent %in% c("narino", "tolima"))
  reg_gr_bio <- region_grupo_data(region, tipo = "biologico", verbose = TRUE, con = con)
  reg_gr_int <- region_grupo_data(region, tipo = "interes", verbose = TRUE,con = con)


  # Temáticas

  tem_list <- tematica_list(region, con = con)
  #tem_list <- NA




  ##
  patrocinadores <- sibdata_patrocinador(con)
  patrocinador <- sibdata_region_patrocinador(con) |>
    filter(slug_region == region)
  patrocinador <- patrocinador |>
    left_join(patrocinadores, by = c("slug_patrocinador" = "slug")) |>
    collect()

  publicadores <- sibdata_region_publicador(con) |>
    filter(slug_region == region) |>
    left_join(sibdata_publicador(con), by = c("slug_publicador" = "slug")) |>
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
  dir.create(file.path("static/data",parent))
  jsonlite::write_json(l, paste0("static/data/",parent,"/",region, ".json"),
                       auto_unbox = TRUE, pretty =TRUE)


})


toc()




