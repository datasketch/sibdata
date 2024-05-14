#library(sibdata)
#library(lfltmagic)

library(tidyverse)

devtools::load_all()


# Generate navigation files

# Generate files for regions




library(tictoc)

here::dr_here()
save_path <- here::here("static", "data")
message("Save path: ", save_path)
#here::set_here("./..")
#setwd("../")
#here::dr_here()
tic()


con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)

region <- "colombia"



message(region)

nav_tematica <- navigation_trees("tematica", con = con)
nav_grupo_biologico <- navigation_trees("grupo_biologico", con = con)
nav_grupo_interes <- navigation_trees("grupo_interes", con = con)
nav_territorio <- navigation_trees("territorio", region = region, con = con)


general_info <- sib_region_general(region, con)

gallery <- make_gallery(region, con)

slides <- make_region_slides(region, con)

reg_gr_bio <- region_grupo_data(region, tipo = "biologico", verbose = TRUE, con)
reg_gr_int <- region_grupo_data(region, tipo = "interes", verbose = TRUE, con)


# Temáticas

tem_list <- tematica_list_col(region, con)
#tem_list <- NA

# Territorio
dir.create(glue::glue("static/charts/{region}"))

subreg_tematica <- subregion_tematica(region, con)
d <- subreg_tematica |>
  collect()

dd <- d |>
  select(slug_region, especies_region_total, registros_region_total)


map_name <- "col_departments2"

deptos <- sibdata_departamento(con) |> collect()
dd <- dd |>
  left_join(deptos, by = c("slug_region" = "slug"), copy = TRUE)


dd_esp <- dd |> select(cod_dane, value = especies_region_total, label) |>
  rename(n_especies = value)
dd_reg <- dd |> select(cod_dane, value = registros_region_total, label) |>
  rename(n_registros = value)
dd_map <- left_join(dd_esp, dd_reg) |>
  select(id = cod_dane, label, n_especies, n_registros)
#tj <- geodato::gd_tj("col_departments")
conmap <- geotable::gt_con()
tj <- geotable::gt_sf("col_departments", conmap) |> left_join(dd_map)
#tj <- geodato::gd_tj("col_departments") |> left_join(dd_map)


region_tipo <- "municipio"
if(region == "colombia") region_tipo <- "departamento"
territorio <- list(
  list(
    slug = "municipios",
    label = "Municipios",
    map_data = tj,
    charts = list(
      list(title = glue::glue("Especies por {region_tipo}"), path = "", layout = "title/chart"),
      list(title =  glue::glue("Registros por {region_tipo}"), path = "", layout = "title/chart")
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
  municipios_lista = list(),
  departamentos_lista = departamentos_lista
)
dir.create(file.path("static/data",region))
jsonlite::write_json(l, paste0("static/data/",region,"/",region, ".json"),
                     auto_unbox = TRUE, pretty =TRUE)
sf::write_sf(tj, paste0("static/data/",region,"/",region, ".geojson"),
             delete_dsn = TRUE)



toc()




