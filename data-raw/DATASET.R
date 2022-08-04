
library(tidyverse)


# Get data form googlesheets
# source("data-raw/get_data.R")

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

ds$grupo_biologico <- ds$grupo_biologico |>
  mutate(icon = slug %in% available_icons)
ds$grupo_interes_conservacion <- ds$grupo_interes_conservacion |>
  mutate(icon = slug %in% available_icons)
ds$tematica <- ds$tematica |>
  mutate(icon = slug %in% available_icons)

region_table <- ds$region
tematica_table <- ds$tematica


#ds$territorio <- ds$ventana_recomendada

# Add indicadores meta data

ind_meta <- read_csv("data-raw/diccionaries/ind_meta.csv")
ds$ind_meta <- ind_meta

#saveRDS(ds, "ds.rds")
#saveRDS(ds, "sib-data-app/ds.rds")

available_tables <- names(ds)

usethis::use_data(ds, available_tables, overwrite = TRUE)
