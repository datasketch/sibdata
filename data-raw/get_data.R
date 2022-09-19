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

# Textos

ss <- "https://docs.google.com/spreadsheets/d/1-FiWbPN7Zn4SQJQ9bhQ542JuZnAzV4FRb-mqUzEDsUM/edit#gid=1694674636"

imagenes <- googlesheets4::read_sheet(ss, sheet = "imagenes_galeria")
destacados_imagenes <- imagenes |>
  select(slug_region, img_link)
write_csv(destacados_imagenes, "data-raw/gallery_images.csv")


preg_frecuentes <- googlesheets4::read_sheet(ss, sheet = "Preguntas frecuentes")
write_csv(preg_frecuentes, "data-raw/preg_frecuentes.csv")

glosario <- googlesheets4::read_sheet(ss, sheet = "Glosario")
write_csv(preg_frecuentes, "data-raw/glosario.csv")






