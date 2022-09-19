
library(tidyverse)


# Get data form googlesheets
source("data-raw/get_data.R")

files <- list.files("data-raw/db-cifras-sib", full.names = TRUE)
table_names <- gsub(".tsv", "",basename(files))

ds <- map(files, read_delim)
names(ds) <- table_names


## Add grupo table

ds$grupo <- ds$grupo_biologico
ds$grupo_biologico <- NULL


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


# Create grupo table

ds$region_grupo_tematica <- ds$region_grupo_biologico |>
  rename(slug_grupo = slug_grupo_biologico)
grupo_tipo <- ds$grupo |> select(slug_grupo = slug, tipo)
ds$region_grupo_tematica <- ds$region_grupo_tematica |>
  left_join(grupo_tipo) |>
  relocate(tipo, .after = slug_grupo)


ds$region_grupo_biologico <- NULL

# Create especie_grupo table

ds$especie_grupo <- ds$especie_grupo_biologico |>
  rename(slug_grupo = slug_grupo_biologico )

ds$especie_grupo_biologico <- NULL

# Add imagenes a destacadas
gallery_images <- read_csv("data-raw/gallery_images.csv")
ds$gallery_images <- gallery_images

# Save

available_tables <- names(ds)

usethis::use_data(ds, available_tables, overwrite = TRUE)


