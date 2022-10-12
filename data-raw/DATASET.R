
library(tidyverse)


# Get data form googlesheets
source("data-raw/get_data.R")

files <- list.files("data-raw/db-cifras-sib", full.names = TRUE)
table_names <- gsub(".tsv", "",basename(files))

ds <- map(files, read_delim)
names(ds) <- table_names

## Add icons
available_icons <- list.files("inst/icons/")
available_icons <- available_icons |>
  tools::file_path_sans_ext() |>
  strsplit("-") |>
  map_chr(1) |>
  unique()

ds$grupo <- ds$grupo |>
  mutate(icon = slug %in% available_icons)
ds$tematica <- ds$tematica |>
  mutate(icon = slug %in% available_icons)

ds$territorio <- read_delim("data-raw/territorio.tsv")

region_table <- ds$region
tematica_table <- ds$tematica


#ds$territorio <- ds$ventana_recomendada

# Add indicadores meta data

ind_meta <- read_csv("data-raw/diccionaries/ind_meta.csv")
ds$ind_meta <- ind_meta

#saveRDS(ds, "ds.rds")
#saveRDS(ds, "inst/sib-data-app/ds.rds")


# Add imagenes a destacadas
gallery_images <- read_csv("data-raw/gallery_images.csv")
ds$gallery_images <- gallery_images

preg_frecuentes <- read_csv("data-raw/preg_frecuentes.csv")
ds$preg_frecuentes <- preg_frecuentes

glosario <- read_csv("data-raw/glosario.csv")
ds$glosario <- glosario


# Clean data

#tmp <- ds$region_tematica
#tmp2  <- tmp |>
#  mutate_at(vars(!matches("slug_region", "fecha_corte")), as.numeric)
#ds$region_tematica <- tmp2
#str(ds$region_tematica)


#readr::write_rds(ds, "data-raw/ds.rds")
saveRDS(ds, "data-raw/ds.rds")

# Save

#available_tables <- names(ds)
#usethis::use_data(ds, available_tables, overwrite = TRUE)

library(duckdb)
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "inst/db/sibdata.duckdb")
map2(ds, names(ds), function(d,nm){
  dbWriteTable(con, nm, d)
})
dbDisconnect(con)


library(RSQLite)
unlink("inst/db/sib.sqlite")
con <- dbConnect(RSQLite::SQLite(), "inst/db/sib.sqlite")
map2(ds, names(ds), function(d,nm){
  dbWriteTable(con, nm, d)
})
dbDisconnect(con)



