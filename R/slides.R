
#' @export
make_region_slides <- function(region){

  sib_validate_available_regions(region, with_grupo = TRUE)


  ####################

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
  esp_tem <- sib_tables("especie_tematica")
  esp_meta <- sib_tables("especie_meta")

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


  slides <- list()

  ##### PREGUNTAS

  # Grupo biológico con mayor número de observaciones

  # reg_gr_bio <- sib_tables("region_grupo_biologico") |>
  #   filter(slug_region == region)

  # Nariño vs Colombia

  reg_vs_parent <- sib_tables("region_tematica") |>
    filter(slug_region %in% c(region, parent))

  d <- reg_vs_parent |>
    select(slug_region, registros_region_total)

  path <- glue::glue("static/charts/{region}/reg_vs_parent.png")

  gg <- sib_chart_waffle(d)
  ggsave(path, gg, width = 4, height = 4)

  x <- d$registros_region_total
  names(x) <- d$slug_region
  x[1] <- x[1] - x[2]
  x <- rev(x)
  x <- round(x/sum(x)*100)
  proportion <- x[1]
  description_tpl <- "El departamento de {region} tiene alrededor del {proportion}% de las observaciones de especies del país."
  title_tpl <- "{region} vs {parent}"
  l <- list(
    id = "slide1",
    layout = "title/(text|chart)",
    title =  toupper(glue::glue(title_tpl)),
    description = glue::glue(description_tpl),
    chart_type = "image",
    chart_url = path
  )
  slides <- list(l)


  # Especies con mayor número de observaciones

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

  x <- glue::glue_data(esp_animal_mas_obs |> slice(1:5), "{species} ({registros})")
  x <- paste(x, collapse = ", ")
  phrase1 <- glue::glue("Las especies de animales con más registros son: {x}.")

  x <- glue::glue_data(esp_planta_mas_obs |> slice(1:5), "{species} ({registros})")
  x <- paste(x, collapse = ", ")
  phrase2 <- glue::glue("Las especies de plantas con más registros son: {x}.")


  l <- list(
    id = "slide2",
    layout = "text-blocks", #text-blocks,
    texts = list(phrase1, phrase2)
  )

  slides <- c(slides, list(l))


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

  t <- n_muni_mas_endemicas
  gt <- sib_chart_gt_table(t,
                     labels = c("Municipio", "Número de especies endémicas"))
  path1 <- glue::glue("static/charts/{region}/muni_mas_endemicas.html")
  gt::gtsave(gt, path1)

  t <- n_muni_mas_amenazadas_global
  gt <- sib_chart_gt_table(t,
                           labels = c("Municipio", "Número de especies amenazadas (global)"))
  path2 <- glue::glue("static/charts/{region}/muni_mas_amenazadas.html")
  gt::gtsave(gt, path2)

  description_tpl <- ""
  title_tpl <- "Los municipios con más: "
  l <- list(
    id = "slide3",
    layout = "title/(chart|chart)",
    title =  toupper(glue::glue(title_tpl)),
    description = glue::glue(description_tpl),
    chart_type = "html",
    chart1_url = path1,
    chart2_url = path2
  )

  slides <- c(slides,list(l))

#
#
#   # ¿Cuál es el municipio con menos registros, ¿por qué?
#
#   muni_menos_esp <- subreg_tematica |>
#     select(slug_region, especies_region_total) |>
#     slice_min(especies_region_total, n = 10)
#
#   muni_mas_esp <- subreg_tematica |>
#     select(slug_region, especies_region_total) |>
#     slice_max(especies_region_total, n = 10)
#
#   muni_menos_reg <- subreg_tematica |>
#     select(slug_region, registros_region_total) |>
#     slice_min(registros_region_total, n = 10)
#
#   muni_mas_esp <- subreg_tematica |>
#     select(slug_region, registros_region_total) |>
#     slice_max(registros_region_total, n = 10)
#
#
#   # Cuántas son las especies estimadas del departamento
#   # ???
#
#   # Cuáles son las especies exóticas/amenazadas/... del departamento
#   # ¿Cuáles especies amenazadas tienen mas/menos observaciones en nariño?
#   # ¿Que especies de peces son comercializadas?
#
#   esp_exoticas <- esp_reg_tem |>
#     filter(grepl("exotica", slug_tematica)) |>
#     distinct(slug_especie) |>
#     left_join(esp_obs) |>
#     arrange(desc(registros))
#
#   esp_amenazadas <- esp_reg_tem |>
#     filter(grepl("amenazada", slug_tematica)) |>
#     distinct(slug_especie) |>
#     left_join(esp_obs) |>
#     arrange(desc(registros))
#
#   esp_cites <- esp_reg_tem |>
#     filter(grepl("cites", slug_tematica)) |>
#     distinct(slug_especie) |>
#     left_join(esp_obs) |>
#     arrange(desc(registros))
#
#   esp_cites_i <- esp_reg_tem |>
#     filter(slug_tematica == "cites-i") |>
#     distinct(slug_especie) |>
#     left_join(esp_obs) |>
#     arrange(desc(registros))
#
#
#   # Cuáles son los municipios con más vacíos de información en el país
#   # = a los municipios con menos registros?
#
#   # Comparación de número especies amenazadas, exóticas, CITES y endémicas de todos los municipios
#   # ... cómparar cómo?
#   # subreg_tematica
#
#   # Cuáles especies tienen más observaciones en pasto
#   # No se puede calcular
#   # esp_reg
#
#
#
#   # ¿Quienes están aportando datos para la región y cuàl es el ranking de esas organizaciones?
#   # ¿Qué porcentaje de datos aporta el top 10% de publicadores del SiB Colombia?
#
#   top_pubs_reg <- pubs_reg |>
#     slice_max(registros, n = 10)
#
#   top_pubs_esp <- pubs_reg |>
#     slice_max(especies, n = 10)
#
#   dist_pubs_por_tipo_n_regs <- pubs_reg |>
#     select(tipo_publicador, registros) |>
#     group_by(tipo_publicador) |>
#     summarise(total = sum(registros))
#
#   dist_pubs_por_tipo <- pubs_reg |>
#     select(tipo_publicador) |>
#     group_by(tipo_publicador) |>
#     summarise(total = n())
#
#
#
#
#
#
#
#   #####################
#











  slides

}

