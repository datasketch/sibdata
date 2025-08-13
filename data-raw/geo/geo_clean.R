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


