library(sibdata)


# generate_navigation
navigation_trees("region",
                 json_file = "static/data/nav_region.json")

navigation_trees("territorio",
                 json_file = "static/data/nav_territorio.json")


navigation_trees("grupo_biologico",
                 json_file = "static/data/nav_grupo_biologico.json")

navigation_trees("grupo_interes_conservacion",
                 json_file = "static/data/nav_grupo_interes_conservacion.json")

navigation_trees("tematica",
                 json_file = "static/data/nav_tematica.json")

# navigation_trees("territorio",
#                  json_file = "static/data/nav_tematica.json")


publicadores_to_json("static/data/publicador.json")


# Copy icons

copy_icons("static")


# Generate files for regions

av_regions <- sib_available_regions()

map(av_regions, function(region){
  # region <- "tolima"
  subregs <- sib_available_subregions(region)
  parent <- sib_parent_region(region)

  reg_gr_bio <- sib_tables("region_grupo_biologico") |>
    dplyr::filter(slug_region == region) |>
    dplyr::mutate(slug = slug_grupo_biologico) |>
    dplyr::relocate(slug)
  reg_gr_int <- sib_tables("region_grupo_interes_conservacion") |>
    dplyr::filter(slug_region == region) |>
    dplyr::mutate(slug = slug_grupo_interes_conservacion) |>
    dplyr::relocate(slug)

  general_info <- sib_region_general(region)

  reg_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == region)
  subreg_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region %in% subregs)
  parent_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == parent)

  esp_reg <- sib_tables("especie_region") |>
    dplyr::filter(slug_region == region)
  esp_tem <- sib_tables("especie_tematica")
  esp_reg_tem <-  esp_reg |>
    dplyr::left_join(esp_tem) |>
    dplyr::select(-registros)


  tem <- sib_tables("tematica") #|>
    #dplyr::filter(is.na(orden))

  d2 <- reg_tematica |>
    dplyr::select(-fecha_corte) |>
    tidyr::pivot_longer(-starts_with("slug"),
                 names_to = c("indicador"),
                 values_to = "count")

  inds <- sib_tables("ind_meta") #|>
    #filter(indicador %in% names(d))
  d3 <- dplyr::left_join(d2, inds)
  d4 <- d3 |>
    dplyr::select_if(~length(unique(.))!= 1) |>
    dplyr::select(-indicador)
  d5 <- d4 %>% dplyr::relocate(count, .after = last_col())
  d6 <- d5 |>
    dplyr::filter(cobertura == "total") |>
    dplyr::filter(tipo == "especies") |>
    dplyr::filter(!is.na(slug_tematica)) |>
    dplyr::select(slug_tematica, label, count)

  tems_list <- d6 |> dplyr::group_split(slug_tematica)
  tem_groups <- d6 |> dplyr::group_by(slug_tematica) |>
    dplyr::group_keys() |> dplyr::pull(slug_tematica)
  #names(tems_list) <- tem_groups

  tematica_list <- purrr::map(tems_list, function(x){
    #x <- tems_list[[8]]
    x$slug <- x$slug_tematica
    esps_tem <- esp_reg_tem |>
      dplyr::select(-slug_region) |>
      dplyr::filter(grepl(x$slug, slug_tematica)) #|>
    #distinct(slug_especie, .keep_all = TRUE)
    #x$especies <- list(esps_tem)
    x$title <- paste0("Lista especies: ", x$slug)
    #chart <- sib_chart_gt_table2(esps_tem)
    slug <- x$slug
    path <- glue::glue("static/charts/{region}/{slug}.html")
    #gt::gtsave(chart, path)
    x$chart <- path
    as.list(x)
  })

  # Territorio
  dir.create(glue::glue("static/charts/{region}"))

  d <- subreg_tematica
  munis_chart1 <- sib_chart_reg_municipios(d, "especies_region_total")
  path1 <- glue::glue("static/charts/{region}/region_municipios_1.html")
  htmlwidgets::saveWidget(munis_chart1, path1)
  munis_chart2 <- sib_chart_reg_municipios(d, "registros_region_total")
  path2 <- glue::glue("static/charts/{region}/region_municipios_2.html")
  htmlwidgets::saveWidget(munis_chart2, path2)

  territorio <- list(
    "municipios" = list(
      charts = list(
        list(title = "Especies por municipio", path = path1),
        list(title = "Observaciones por municipio", path = path2)
      )
    ),
    "reserva-forestal-la-planada" = list(
      list(title = "Próximamente tendrás acceso a la información de la reserva", path = NULL)
    ),
    "resguardo-indigena-pialapi-pueblo-viejo" = list(
      list(title = "Próximamente tendrás acceso a la información del resguardo", path = NULL)
    )
  )

  slides <- make_region_slides(region)

  l <- list(
    general_info = general_info,
    slides = slides,
    grupos_biologicos = reg_gr_bio,
    grupos_interes = reg_gr_int,
    tematica = tematica_list,
    territorio = territorio
    )
  jsonlite::write_json(l, paste0("static/data/",region, ".json"),
                   auto_unbox = TRUE, pretty =TRUE)
})






