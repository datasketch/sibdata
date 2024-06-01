
library(tidyverse)

options(timeout = 1200)
# Get ds-cifras-sib from gitlab
# https://gitlab.com/sib-colombia/cifras-biodiversidad.git

url <- "https://gitlab.com/sib-colombia/cifras-biodiversidad/-/archive/main/cifras-biodiversidad-main.zip?path=db-cifras-sib"
if(!dir.exists("data-raw/downloads")) dir.create("data-raw/downloads")
download.file(url, "data-raw/downloads/db-sifras-sib.zip")
unzip("data-raw/downloads/db-sifras-sib.zip",
      exdir = "data-raw/downloads")
fs::dir_copy("data-raw/downloads/cifras-biodiversidad-main-db-cifras-sib/db-cifras-sib/",
             "data-raw/db-cifras-sib", overwrite = TRUE)
unlink("data-raw/downloads",
       recursive = TRUE)


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



# Add imagenes a destacadas
gallery_images <- read_csv("data-raw/gallery_images.csv")
ds$gallery_images <- gallery_images

banner_images <- read_csv("data-raw/banner_images.csv")
ds$banner_images <- banner_images

preg_frecuentes <- read_csv("data-raw/preg_frecuentes.csv")
ds$preg_frecuentes <- preg_frecuentes

glosario <- read_csv("data-raw/glosario.csv")
ds$glosario <- glosario


#saveRDS(ds, "data-raw/ds.rds")
#usethis::use_data(ds, internal = TRUE, overwrite = TRUE)


# Save

#available_tables <- names(ds)
#usethis::use_data(ds, available_tables, overwrite = TRUE)

library(RSQLite)
unlink("inst/db/sibdata.sqlite")
con <- dbConnect(RSQLite::SQLite(), "inst/db/sibdata.sqlite")
map2(ds, names(ds), function(d,nm){
  dbWriteTable(con, nm, d)
})
dbDisconnect(con)


library(duckdb)
unlink("inst/db/sibdata.duckdb")
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "inst/db/sibdata.duckdb")
map2(ds, names(ds), function(d,nm){
  dbWriteTable(con, nm, d, overwrite = TRUE)
})
dbListTables(con)
duckdb::dbDisconnect(con, shutdown = TRUE)
# duckdb::duckdb_shutdown(con)






