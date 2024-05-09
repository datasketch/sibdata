#' @export
make_region_slides2 <- function(region, con = con){

  #sib_validate_available_regions(region)


  #dir.create(glue::glue("static/charts/{region}"))
  ####################

  #subregs <- sib_available_subregions(region)
  parent <- sib_parent_region(region, con)
  if(parent %in% c("reservas-forestales-protectoras",
                   "territorios-indigenas"))
    parent <- "narino"

  reg_labels <- sib_region_labels(con) |> collect()

  reg_gr_bio <- sibdata_region_grupo(con) |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(tipo == "biologico") |>
    collect()

  reg_gr_int <- sibdata_region_grupo(con) |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(tipo == "interes") |>
    collect()

  reg_tematica <- sibdata_region_tematica(con) |>
    dplyr::filter(slug_region == region) |>
    collect()
  # subreg_tematica <- sibdata_region_tematica() |>
  #   collect() |>
  #   dplyr::filter(slug_region %in% subregs) |>
  #   dplyr::left_join(reg_labels, by = c("slug_region" = "slug")) |>
  #   relocate(label)
  parent_tematica <- sibdata_region_tematica(con) |>
    collect() |>
    dplyr::filter(slug_region == parent)

  esp <- sibdata_especie(con) |> collect()
  esp_tem <- sibdata_especie_tematica(con) |> collect()
  esp_meta <- sibdata_especie_meta(con) |> collect()

  esp_reg <- sibdata_especie_region(con) |>
    dplyr::filter(slug_region == region) |>
    collect()

  esp_parent <- sibdata_especie_region(con) |>
    dplyr::filter(slug_region == parent) |>
    collect()

  esp_reg_tem <-  esp_reg |>
    dplyr::left_join(esp_tem, by = c("slug_region", "slug_especie")) |>
    dplyr::select(-registros)

  pubs <-  sibdata_publicador(con)
  pubs_reg <- sibdata_region_publicador(con) |>
    dplyr::filter(slug_region == region) |>
    dplyr::distinct() |>
    dplyr::left_join(pubs |> select(slug, label, pais_publicacion, tipo_publicador),
                     by = c("slug_publicador" = "slug")) |>
    dplyr::select(label, pais_publicacion, tipo_publicador,
                  registros, especies) |>
    collect()

  estimada <- sibdata_estimada(con) |> collect()


  slides <- list()

  ##### PREGUNTAS

  # Grupo biológico con mayor número de observaciones

  # reg_gr_bio <- sib_tables("region_grupo_biologico") |>
  #   filter(slug_region == region)

  # Nariño vs Colombia

  if(region == "colombia"){
    l <- NULL
  }else{
    reg_vs_parent <- sibdata_region_tematica(con) |>
      dplyr::filter(slug_region %in% c(region, parent)) |>
      collect()

    deptos <- c("tolima", "boyaca", "narino", "santander")
    d <- reg_vs_parent |>
      dplyr::select(slug_region, especies_region_total)
    idx_col <- which(d$slug_region %in% deptos)

    waffle <- d

    # if(idx_col == 1){
    #   d <- d |> slice(2:1)
    # }
    #
    # path <- glue::glue("static/charts/{region}/reg_vs_parent.png")
    #
    # gg <- sib_chart_waffle(d)
    # ggsave(path, gg, width = 4, height = 4)
    #
    # x <- d$especies_region_total
    # names(x) <- d$slug_region
    # x[2] <- x[2] - x[1]
    # x <- rev(x)
    # x <- round(x/sum(x)*100)
    # proportion <- x[2]

    proportion <- waffle$especies_region_total[2]/waffle$especies_region_total[1]
    proportion <- round(proportion*100, digits = 1)

    regionLabel <- sib_merge_region_label(data.frame(slug_region = region),con = con)$label
    parentLabel <- sib_merge_region_label(data.frame(slug_region = parent),con = con)$label
    regionTitle <- makeup::makeup_chr(region, "Title")

    esp_parent <- reg_vs_parent |>
      filter(slug_region == parent) |> pull(especies_region_total)
    esp_parent_str <- makeup::makeup(esp_parent,"45.343,00")
    esp_muni <- reg_vs_parent |>
      filter(slug_region != parent) |> pull(especies_region_total)
    esp_muni_str <- makeup::makeup(esp_muni,"45.343,00")
    esp_muni_endemicas <- reg_vs_parent |>
      filter(slug_region != parent) |> pull(especies_endemicas)
    esp_muni_endemicas_str <- makeup::makeup(esp_muni_endemicas,"45.343,00")

    description_tpl <- "De las {esp_parent_str} especies observadas en {parentLabel},
    el municipio de {regionLabel} aporta {esp_muni_str}, equivalentes a {proportion}%.
    De estas {esp_muni_endemicas_str} especies son endémicas."

    title_tpl <- "¿Cómo está {regionLabel} frente al resto de {parentLabel}?"

    if(region %in% c("reserva-forestal-la-planada",
                     "resguardo-indigena-pialapi-pueblo-viejo"))
      description_tpl <- "De las {esp_parent_str} especies observadas en {parentLabel},
    {regionLabel} aporta {esp_muni_str}, equivalentes a {proportion} %.
    De estas {esp_muni_endemicas_str} especies son endémicas."


    l <- list(
      id = "slide1",
      layout = "title/(text|chart)",
      title =  glue::glue(title_tpl),
      description = glue::glue(description_tpl),
      # chart_type = "image",
      # chart_url = path,
      waffle = waffle
    )
    slides <- list(l)
  }



  # Especies con mayor número de observaciones

  esp_obs <- esp_reg |>
    left_join(esp_meta, by = c("slug_especie" = "slug")) |>
    left_join(esp, by = c("slug_especie" = "slug"))

  esp_animal_mas_obs <- esp_obs |>
    filter(kingdom == "Animalia") |>
    slice_max(registros, n = 20) |>
    mutate(registros_str = makeup::makeup(as.numeric(registros), "45.343,00"))

  esp_animal_menos_obs <- esp_obs |>
    filter(kingdom == "Animalia") |>
    slice_min(registros, n = 20) |>
    mutate(registros_str = makeup::makeup(as.numeric(registros), "45.343,00"))

  esp_planta_mas_obs <- esp_obs |>
    filter(kingdom == "Plantae") |>
    slice_max(registros, n = 20) |>
    mutate(registros_str = makeup::makeup(as.numeric(registros),  "45.343,00"))

  esp_planta_menos_obs <- esp_obs |>
    filter(kingdom == "Plantae") |>
    slice_min(registros, n = 20) |>
    mutate(registros_str = makeup::makeup(as.numeric(registros),  "45.343,00"))

  esp_mamiferos_mas_obs <- esp_obs |>
    filter(class == "Mammalia") |>
    slice_min(registros, n = 20) |>
    mutate(registros_str = makeup::makeup(as.numeric(registros),  "45.343,00"))


  x <- glue::glue_data(esp_animal_mas_obs |> slice(1:5), "_{species}_ ({registros_str})")
  x <- paste(x, collapse = ", ")
  phrase1 <- glue::glue("Las especies de animales con más registros son: {x}.")

  x <- glue::glue_data(esp_planta_mas_obs |> slice(1:5), "_{species}_ ({registros_str})")
  x <- paste(x, collapse = ", ")
  phrase2 <- glue::glue("Las especies de plantas con más registros son: {x}.")


  l <- list(
    id = "slide2",
    layout = "text-blocks", #text-blocks,
    texts = list(phrase1, phrase2)
  )

  slides <- c(slides, list(l))


  # # ¿Cuál es el municipio con mayor número de especies marinas, endémicas, amenazadas?
  #
  # n_muni_mas_marinas <- subreg_tematica |>
  #   select(slug_region, label, especies_marinas) |>
  #   filter(!is.na(especies_marinas)) |>
  #   slice_max(n = 10, order_by = especies_marinas) |>
  #   select(label, n = especies_marinas)
  #
  # n_muni_mas_endemicas <- subreg_tematica |>
  #   select(slug_region, label, especies_endemicas) |>
  #   filter(!is.na(especies_endemicas)) |>
  #   slice_max(n = 10, order_by = especies_endemicas) |>
  #   select(label, n = especies_endemicas)
  #
  # n_muni_mas_amenazadas_global <- subreg_tematica |>
  #   select(slug_region, label, especies_amenazadas_global_total) |>
  #   filter(!is.na(especies_amenazadas_global_total)) |>
  #   slice_max(n = 10, order_by = especies_amenazadas_global_total) |>
  #   select(label, n = especies_amenazadas_global_total)
  #
  #
  # n_muni_mas_amenazadas_nacional <- subreg_tematica |>
  #   select(slug_region, label, especies_amenazadas_nacional_total) |>
  #   filter(!is.na(especies_amenazadas_nacional_total)) |>
  #   slice_max(n = 10, order_by = especies_amenazadas_nacional_total) |>
  #   select(label, n = especies_amenazadas_nacional_total)
  #
  # t <- n_muni_mas_endemicas
  # gt <- sib_chart_gt_table(t,
  #                          labels = c("Municipio", "Número de especies endémicas"),
  #                          color = "#34d986")
  # path1 <- glue::glue("static/charts/{region}/muni_mas_endemicas.html")
  # gt::gtsave(gt, path1)
  #
  # t <- n_muni_mas_amenazadas_nacional
  # gt <- sib_chart_gt_table(t,
  #                          labels = c("Municipio", "Número de especies amenazadas (nacional)"),
  #                          color = "#f59542"
  # )
  # path2 <- glue::glue("static/charts/{region}/muni_mas_amenazadas.html")
  # gt::gtsave(gt, path2)
  #
  # description_tpl <- ""
  # title_tpl <- "Los municipios con más: "
  # l <- list(
  #   id = "slide3",
  #   layout = "title/(chart|chart)",
  #   title =  toupper(glue::glue(title_tpl)),
  #   description = glue::glue(description_tpl),
  #   chart_type = "html",
  #   chart1_url = path1,
  #   chart2_url = path2
  # )
  #
  # slides <- c(slides,list(l))

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
