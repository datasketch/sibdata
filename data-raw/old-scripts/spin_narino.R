library(sibdata)

library(highcharter)
library(waffle)


region <- "narino"
sib_available_tables()

# [1] "dato_relevante"                          "departamento"
# [3] "especie_grupo_biologico"                 "especie_grupo_interes_conservacion"
# [5] "especie_meta"                            "especie_region"
# [7] "especie_tematica"                        "especie"
# [9] "estimada"                                "grupo_biologico"
# [11] "grupo_interes_conservacion"              "municipio"
# [13] "patrocinador"                            "publicador"
# [15] "ranking"                                 "referencia_estimada"
# [17] "region_grupo_biologico"                  "region_grupo_interes_conservacion"
# [19] "region_patrocinador"                     "region_publicador"
# [21] "region_tematica"                         "region"
# [23] "tematica"                                "temporalidad_grupo_biologico"
# [25] "temporalidad_grupo_interes_conservacion" "temporalidad_pais_publicacion"
# [27] "temporalidad_publicador_region"          "temporalidad_region"
# [29] "ventana_recomendada"                     "ind_meta"

subregs <- sib_available_subregions(region)
parent <- sib_parent_region(region)

reg_gr_bio <- sib_tables("region_grupo_biologico") |>
  filter(slug_region == region)
reg_gr_int <- sib_tables("region_grupo_interes_conservacion") |>
  filter(slug_region == region)
reg_tematica <- sib_tables("region_tematica") |>
  filter(slug_region == region)
subreg_tematica <- sib_tables("region_tematica") |>
  filter(slug_region %in% subregs)
parent_tematica <- sib_tables("region_tematica") |>
  filter(slug_region == parent)

esp <- sib_tables("especie")

esp_reg <- sib_tables("especie_region") |>
  filter(slug_region == region)

esp_parent <- sib_tables("especie_region") |>
  filter(slug_region == parent)

#esp_subreg <- sib_tables("especie_region") |> ## No se puede calcular
#  filter(slug_region %in% subregs)

esp_reg_tem <-  esp_reg |>
  left_join(esp_tem) |>
  select(-registros)

pubs <-  sib_tables("publicador")
pubs_reg <- sib_tables("region_publicador")|>
  filter(slug_region == region) |>
  distinct() |>
  left_join(pubs, by = c("slug_publicador" = "slug")) |>
  select(label, pais_publicacion, tipo_publicador, registros, especies)

estimada <- sib_tables("estimada")


##### PREGUNTAS

# Grupo biológico con mayor número de observaciones
# reg_gr_bio <- sib_tables("region_grupo_biologico") |>
#   filter(slug_region == region)


# Nariño vs Colombia

reg_vs_parent <- sib_tables("region_tematica") |>
  filter(slug_region %in% c(region, parent))

x <- reg_vs_parent$registros_region_total
names(x) <- reg_vs_parent$slug_region
x[1] <- x[1] - x[2]
x <- rev(x)
x <- round(x/sum(x)*100)

gg <- waffle(x, colors = c("orange", "lightgrey"), row = 10,
       flip = TRUE) +
  theme(legend.position = "none")
ggsave("static/charts/reg_vs_parent.png", gg, width = 4, height = 4)



# Especie con mayor número de observaciones

esp_obs <- esp_reg |>
  left_join(esp_meta, by = c("slug_especie" = "slug")) |>
  left_join(esp, by = c("slug_especie" = "slug"))

esp_animal_mas_obs <- esp_obs |>
  filter(kingdom == "Animalia") |>
  slice_max(registros, n = 20)

esp_animal_menos_obs <- esp_obs |>
  filter(kingdom == "Animalia") |>
  slice_min(registros, n = 20)

esp_planta_mas_obs <- esp_obs |>
  filter(kingdom == "Plantae") |>
  slice_max(registros, n = 20)

esp_planta_menos_obs <- esp_obs |>
  filter(kingdom == "Plantae") |>
  slice_min(registros, n = 20)

esp_mamiferos_mas_obs <- esp_obs |>
  filter(class == "Mammalia") |>
  slice_min(registros, n = 20)

# ¿Cuál es el municipio con mayor número de especies marinas, endémicas, amenazadas?

n_muni_mas_marinas <- subreg_tematica |>
  select(slug_region, especies_marinas) |>
  filter(!is.na(especies_marinas)) |>
  slice_max(n = 10, order_by = especies_marinas)

n_muni_mas_endemicas <- subreg_tematica |>
  select(slug_region, especies_endemicas) |>
  filter(!is.na(especies_endemicas)) |>
  slice_max(n = 10, order_by = especies_endemicas)

n_muni_mas_amenazadas_global <- subreg_tematica |>
  select(slug_region, especies_amenazadas_global_total) |>
  filter(!is.na(especies_amenazadas_global_total)) |>
  slice_max(n = 10, order_by = especies_amenazadas_global_total)



# ¿Cuál es el municipio con menos registros, ¿por qué?

muni_menos_esp <- subreg_tematica |>
  select(slug_region, especies_region_total) |>
  slice_min(especies_region_total, n = 10)

muni_mas_esp <- subreg_tematica |>
  select(slug_region, especies_region_total) |>
  slice_max(especies_region_total, n = 10)

muni_menos_reg <- subreg_tematica |>
  select(slug_region, registros_region_total) |>
  slice_min(registros_region_total, n = 10)

muni_mas_esp <- subreg_tematica |>
  select(slug_region, registros_region_total) |>
  slice_max(registros_region_total, n = 10)


# Cuántas son las especies estimadas del departamento
# ???

# Cuáles son las especies exóticas/amenazadas/... del departamento
# ¿Cuáles especies amenazadas tienen mas/menos observaciones en nariño?
# ¿Que especies de peces son comercializadas?

esp_exoticas <- esp_reg_tem |>
  filter(grepl("exotica", slug_tematica)) |>
  distinct(slug_especie) |>
  left_join(esp_obs) |>
  arrange(desc(registros))

esp_amenazadas <- esp_reg_tem |>
  filter(grepl("amenazada", slug_tematica)) |>
  distinct(slug_especie) |>
  left_join(esp_obs) |>
  arrange(desc(registros))

esp_cites <- esp_reg_tem |>
  filter(grepl("cites", slug_tematica)) |>
  distinct(slug_especie) |>
  left_join(esp_obs) |>
  arrange(desc(registros))

esp_cites_i <- esp_reg_tem |>
  filter(slug_tematica == "cites-i") |>
  distinct(slug_especie) |>
  left_join(esp_obs) |>
  arrange(desc(registros))


# Cuáles son los municipios con más vacíos de información en el país
# = a los municipios con menos registros?

# Comparación de número especies amenazadas, exóticas, CITES y endémicas de todos los municipios
# ... cómparar cómo?
# subreg_tematica

# Cuáles especies tienen más observaciones en pasto
# No se puede calcular
# esp_reg



# ¿Quienes están aportando datos para la región y cuàl es el ranking de esas organizaciones?
# ¿Qué porcentaje de datos aporta el top 10% de publicadores del SiB Colombia?

top_pubs_reg <- pubs_reg |>
  slice_max(registros, n = 10)

top_pubs_esp <- pubs_reg |>
  slice_max(especies, n = 10)

dist_pubs_por_tipo_n_regs <- pubs_reg |>
  select(tipo_publicador, registros) |>
  group_by(tipo_publicador) |>
  summarise(total = sum(registros))

dist_pubs_por_tipo <- pubs_reg |>
  select(tipo_publicador) |>
  group_by(tipo_publicador) |>
  summarise(total = n())





