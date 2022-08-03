
library(tidyverse)


# Get data form googlesheets
# source("data-raw/get_data.R")

files <- list.files("data-raw/db-cifras-sib", full.names = TRUE)
table_names <- gsub(".tsv", "",basename(files))

ds <- map(files, read_delim)
names(ds) <- table_names

ind_meta <- read_csv("data-raw/diccionaries/ind_meta.csv")
ds$ind_meta <- ind_meta

#saveRDS(ds, "ds.rds")
#saveRDS(ds, "sib-data-app/ds.rds")

available_tables <- names(ds)

usethis::use_data(ds, available_tables, overwrite = TRUE)
