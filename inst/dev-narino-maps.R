
library(sibdata)
library(tidyverse)

region <- "narino"
sib_available_tables()

subregs <- sib_available_subregions(region)
parent <- sib_parent_region(region)

reg_gr_bio <- sib_tables("region_grupo_biologico") |>
  filter(slug_region == region)

subreg_gr_bio <- sib_tables("region_grupo_biologico") |>
  filter(slug_region %in% subregs)

reg_gr_int <- sib_tables("region_grupo_interes_conservacion") |>
  filter(slug_region == region)
reg_tematica <- sib_tables("region_tematica") |>
  filter(slug_region == region)
subreg_tematica <- sib_tables("region_tematica") |>
  filter(slug_region %in% subregs)
parent_tematica <- sib_tables("region_tematica") |>
  filter(slug_region == parent)

municipios <- sib_tables("municipio")

munis <- subreg_tematica |> select(-fecha_corte)

munis <- munis |>
  left_join(municipios, by = c("slug_region" = "slug")) |>
  select(-fecha_corte) |>
  relocate(slug_region, label, cod_dane, marino)

write_csv(munis, "inst/municipios-indicadores.csv")
openxlsx::write.xlsx(munis, "inst/municipios-indicadores.xlsx", overwrite = TRUE)

munis2 <- subreg_gr_bio




