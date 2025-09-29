library(sf)

# region-amazonia
## Map shaper, simplify REGION_AMAZONIA save as data-raw/geo/region-amazonia.geo.json
geo_path <- "data-raw/geo/region-amazonia.geo.json"
sf <- st_read(sys_file_sibdata(geo_path), quiet = TRUE)
sf <- sf |> select(id = dpto_ccdgo, label = dpto_cnmbr, slug_region = slug_regio)
st_write(sf, "inst/geo/region-amazonia.geojson")

sf_amazonia <- sf
geo_path <- "static/data/colombia/colombia.geojson"
sf <- st_read(geo_path, quiet = TRUE)
sf <- sf |> filter(sf$id %in% sf_amazonia$id)
st_write(sf, "inst/geo/region-amazonia-departamentos.geojson")

#municipios amazonia
geo_path <- "data-raw/geo/region-amazonia-municipios.geo.json"
sf <- st_read(sys_file_sibdata(geo_path), quiet = TRUE) |>
  select(cod_dane = mpio_ccdgo,
         cod_dane_depto = dpto_ccdgo,
         municipio = mpio_cnmbr) |>
  mutate(cod_dane = paste0(cod_dane_depto, cod_dane)) |>
  select(cod_dane, cod_dane_depto, municipio)
munis_amazonia_cod_dane <- sf$cod_dane
con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)
deptos <- unique(sf$cod_dane_depto)
munis <- sibdata_municipio(con) |> collect() |>
  mutate(depto_id = str_sub(cod_dane,1,2)) |>
  filter(cod_dane %in% munis_amazonia_cod_dane) |>
  select(slug, label, depto_id)
deptos_keys <- munis |> group_by(depto_id) |> group_keys() |> pull()
munis_list <- munis |>
  group_split(depto_id)
names(munis_list) <- deptos_keys
munis_list <- map(munis_list, as.data.frame)
jsonlite::write_json(munis_list, "inst/geo/region-amazonia-municipios.json")

# resguardo-indigena-pialapi-pueblo-viejo
geo_path <- "data-raw/geo/Resguardo"
sf <- st_read(sys_file_sibdata(geo_path), quiet = TRUE)
sf <- sf |> select(id = RICODIGO, label = RINOMBRE) |>
  mutate(slug_region = "resguardo-indigena-pialapi-pueblo-viejo")
unlink("inst/geo/resguardo-indigena-pialapi-pueblo-viejo.geojson")
st_write(sf, "inst/geo/resguardo-indigena-pialapi-pueblo-viejo.geojson")

# reserva-forestal-la-planada

geo_path <- "data-raw/geo/Reserva/"
sf <- st_read(sys_file_sibdata(geo_path), quiet = TRUE)
sf <- sf |> select(id = id_pnn, label = nombre) |>
  mutate(slug_region = "reserva-forestal-la-planada")
unlink("inst/geo/reserva-forestal-la-planada.geojson")
st_write(sf, "inst/geo/reserva-forestal-la-planada.geojson")


