library(tidyverse)
library(googlesheets4)


## Dictionary from Googlesheets

#googlesheets4::gs4_deauth()
#googlesheets4::gs4_auth()
ss <- "https://docs.google.com/spreadsheets/d/1m1OAQ6Xhn-gHbIoRSHlK_MFN8aFxsSuQwy4OgIJhftQ/edit#gid=0"
tables <- googlesheets4::read_sheet(ss)
dic <- googlesheets4::read_sheet(ss, sheet = "diccionario")
ind_meta <- googlesheets4::read_sheet(ss, sheet = "indicadores_meta")

write_csv(tables, "data-raw/diccionaries/tables.csv")
write_csv(dic, "data-raw/diccionaries/dic.csv")
write_csv(ind_meta, "data-raw/diccionaries/ind_meta.csv")


