#library(sibdata)
#library(lfltmagic)
library(tictoc)

library(vctrs)
devtools::load_all()


here::dr_here()
save_path <- here::here("static", "data")
message("Save path: ", save_path)
#here::set_here("./..")
# setwd("../")
# here::dr_here()

str(getwd())
tic()


con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)
av_regions <- unname(sib_available_regions(subtipo = c("Departamento"), con = con))


av_regions_top <- c("boyaca","narino","tolima", "santander")
av_regions <- av_regions[!av_regions %in% av_regions_top]
#av_regions <- c("boyaca","narino","tolima", "santander")

map(av_regions[1:4], safely(function(region){
  message("##################")
  message("\n", region, "\n")

  # region <- av_regions[4]
  # region <- "risaralda"
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

  #slides <- list()
  slides <- make_region_slides(region, con, save_path = save_path)


  reg_gr_bio <- region_grupo_data(region, tipo = "biologico", verbose = TRUE, con = con)
  #reg_gr_bio <- list()
  reg_gr_int <- region_grupo_data(region, tipo = "interes", verbose = TRUE, con = con)
  #reg_gr_int <- list()


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

  region_nm <- region
  if(region == "narino") region_nm <- "NARIÑO"

  dd_map <- left_join(dd_esp, dd_reg) |>
    select(id = cod_dane, label, n_especies, n_registros)
  tj <- geodato::gd_tj("col_municipalities") |>
    filter(depto == toupper(region_nm))
  tj <- tj |> left_join(dd_map)


  message("Message Territorio")

  territorio <- list(
    list(
      slug = "municipios",
      label = "Municipios",
      map_data = tj,
      charts = list(
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

  message("Patrocinadores")

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

  message("Before creating the list")

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
  message("Creating dir:")
  message(glue::glue("static/data/{region}"))
  dir.create(glue::glue("static/data/{region}"))
  if(!dir.exists(file.path(save_path,region))){
    dir.create(file.path(save_path,region))
  }
  jsonlite::write_json(l, paste0(save_path,"/",region,"/",region, ".json"),
                       auto_unbox = TRUE, pretty =TRUE)



}))


toc()
|>



