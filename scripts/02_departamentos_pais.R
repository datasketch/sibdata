#library(sibdata)
#library(lfltmagic)
library(tictoc)
library(geotable)
library(vctrs)
devtools::load_all()


# con_map <- gt_con()
# map_name <- "col_municipalities_boyaca"
# sf2 <- gt_sf(map_name, con_map)
# sf::write_sf(sf2, "~/Downloads/boyaca.geojson")
# sf::write_sf(tj, "~/Downloads/boyaca.geojson")



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
av_regions_territorio <- c(
  "reserva-forestal-la-planada",
  "resguardo-indigena-pialapi-pueblo-viejo"
)
#av_regions <- av_regions[!av_regions %in% av_regions_top]
#av_regions <- c("boyaca","narino","tolima", "santander")

av_regions <- c(av_regions, av_regions_territorio)


map(av_regions, safely(function(region){
  message("##################")
  message("\n", region, "\n")

  # region <- av_regions[4]
  # region <- "amazonas"
  # region <- "risaralda"
  # region <- "boyaca"
  # region <- "narino"
  # region <- "tolima"
  # region <- "santander"
  # region <- "atlantico"
  # region <- "bogota-dc"
  # region <- "norte-santander"
  # region <- "reserva-forestal-la-planada"
  # region <- "resguardo-indigena-pialapi-pueblo-viejo"

  nav_tematica <- navigation_trees("tematica", con = con)
  nav_grupo_biologico <- navigation_trees("grupo_biologico", con = con)
  nav_grupo_interes <- navigation_trees("grupo_interes", con = con)

  nav_territorio <- navigation_trees("territorio", region = region, con = con)


  general_info <- sib_region_general(region, con)

  gallery <- list()
  if(region %in% av_regions_top){
    gallery <- make_gallery(region, con)
  }

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
  subreg_tematica <- subregion_tematica(region, con)
  d <- subreg_tematica |>
    collect()

  dd <- d |>
    select(slug_region, especies_region_total, registros_region_total)



  munis <- sibdata_municipio(con) |> collect()
  dd <- dd |>
    left_join(munis, by = c("slug_region" = "slug"))

  dd_esp <- dd |> select(cod_dane, value = especies_region_total, label, slug_region) |>
    rename(n_especies = value)
  dd_reg <- dd |> select(cod_dane, value = registros_region_total, label, slug_region) |>
    rename(n_registros = value)

  region_id <- region
  region_id <- gsub("-", "_", region_id)

  # geotable::gt_sf("col_municipalities_vaupes")
  if(region_id == "norte_santander") region_id <- "norte_de_santander"
  if(region_id == "san_andres_providencia") region_id <- "archipielago_de_san_andres_providencia_y_santa_catalina"
  #if(region_id == "bogota-dc") region_nm <- "BOGOTÁ D.C."
  # if(region == "atlantico") region_nm <- "ATLÁNTICO"
  # if(reion == "vaupes") region_nm <- "VAUPÉS"
  # if(region == "valle-del-cauca") region_nm <- "VALLE DEL CAUCA"
  # if(region == "la-guajira") region_nm <- "LA GUAJIRA"
  map_name <- paste0("col_municipalities_", region_id)

  dd_map <- left_join(dd_esp, dd_reg) |>
    select(id = cod_dane, label, n_especies, n_registros)

  conmap <- geotable::gt_con()
  tj <- geotable::gt_sf(map_name, con = conmap) |>
    select(-name)
#
#   tj <- geodato::gd_tj("col_municipalities") |>
#     filter(depto == toupper(region_nm))
  tj <- tj |> left_join(dd_map, by = "id")

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
    left_join(sibdata_publicador(con) |> select(-especies, -registros),
              by = c("slug_publicador" = "slug")) |>
    collect()
  publicadores_tipo <- publicadores |>
    select(tipo_organizacion, registros) |>
    mutate(tipo_organizacion = ifelse(is.na(tipo_organizacion), "No definido", tipo_organizacion)) |>
    summarise(n_tipo = n(),
              n_tipo_obs = sum(registros),
              .by = tipo_organizacion) |>
    mutate(pct_tipo = n_tipo/sum(n_tipo),
           pct_tipo_obs = n_tipo_obs/sum(n_tipo_obs))


  publicadores_list <- publicadores |>
    select(slug_publicador, registros = registros, especies = especies,
           label, pais_publicacion,
           url_logo, url_socio) |>
    arrange(desc(registros))

  publicadores <- list(
    publicadores_tipo = publicadores_tipo,
    publicadores_list = publicadores_list
  )


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
  sf::write_sf(tj, paste0(save_path,"/",region,"/",region, ".geojson"),
               delete_dsn = TRUE)
  opts <- list(main_border_width = 0.1,
               main_border_color = "#007139",
               fill_color = "#b3cfc0",
               minor_border_color = "#007139",
               minor_border_width = 0.1)
  gt_icon(map_name, opts = opts,
          save_path = paste0(save_path,"/",region,"/",region, ".svg"))

}))


toc()



