#library(sibdata)
#library(lfltmagic)
library(tictoc)

library(vctrs)
devtools::load_all()


# here::dr_here()
# #here::set_here("./..")
# setwd("../")
# here::dr_here()

str(getwd())
tic()


con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)
av_regions <- sib_available_regions(subtipo = c("Departamento"), con = con)


av_regions <- c("boyaca","narino","tolima", "santander")

map(av_regions, function(region){
  message("##################")
  message("\n", region, "\n")

  # region <- "boyaca"
  # region <- "narino"
  # region <- "tolima"
  # region <- "santander"

  nav_tematica <- navigation_trees("tematica", con = con)
  nav_grupo_biologico <- navigation_trees("grupo_biologico", con = con)
  nav_grupo_interes <- navigation_trees("grupo_interes", con = con)
  nav_territorio <- navigation_trees("territorio", region = region, con = con)


  general_info <- sib_region_general(region, con)

  gallery <- make_gallery(region, con)

  slides <- make_region_slides(region, con)
  #slides <- list()

  reg_gr_bio <- region_grupo_data(region, tipo = "biologico", verbose = TRUE, con = con)
  reg_gr_int <- region_grupo_data(region, tipo = "interes", verbose = TRUE, con = con)


  # Temáticas

  tem_list <- tematica_list(region, con = con)
  #tem_list <- NA




  # Territorio
  dir.create(glue::glue("static/charts/{region}"))
  library(ltgeo)
  subreg_tematica <- subregion_tematica(region, con)
  d <- subreg_tematica |>
    collect()

  dd <- d |>
    select(slug_region, especies_region_total, registros_region_total)


  map_name <- paste0("col_municipalities_", region)
  munis <- sibdata_municipio(con) |> collect()
  dd <- dd |>
    left_join(munis, by = c("slug_region" = "slug"))

  dd_esp <- dd |> select(cod_dane, value = especies_region_total, label, slug_region) |>
    rename(n_especies = value)
  dd_reg <- dd |> select(cod_dane, value = registros_region_total, label, slug_region) |>
    rename(n_registros = value)

  dd_map <- left_join(dd_esp, dd_reg) |>
    select(id = cod_dane, label, n_especies, n_registros)
  tj <- geodato::gd_tj("col_municipalities") |>
    filter(depto == toupper(region))
  tj <- tj |> left_join(dd_map)

  territorio <- list(
    list(
      slug = "municipios",
      label = "Municipios",
      map_data = tj,
      charts = list(
        list(title = glue::glue("Especies por {region_tipo}"), path = "", layout = "title/chart"),
        list(title =  glue::glue("Observaciones por {region_tipo}"), path = "", layout = "title/chart")
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



  municipios_lista <- subreg_tematica |>
    select(slug =slug_region, label) |>
    collect()
  departamentos_lista <- tribble(
    ~slug, ~label,
    "boyaca", "Boyacá",
    "narino", "Nariño",
    "santander", "Santander",
    "tolima", "Tolima"
  )

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
    municipios_lista = municipios_lista,
    departamentos_lista = departamentos_lista
  )
  dir.create(file.path("static/data",region))
  jsonlite::write_json(l, paste0("static/data/",region,"/",region, ".json"),
                       auto_unbox = TRUE, pretty =TRUE)



})


toc()




