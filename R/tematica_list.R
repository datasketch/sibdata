
tematica_list <- function(region, con){

  # region <- "boyaca"
  #reg_tematica <- region_tematica(region)

  # esp_label <- especie_label()
  # esp_reg <- especie_region(region)
  # esp_reg_tem <- especie_region_tematica(region)
  # tem <- sib_tables("tematica") #|>
  #region_tem_long <- region_tematica_long(region)

  parent_region <- sib_parent_region(region, con)

  estimadas <- sibdata_estimada(con) |> collect() |>
    filter(slug_grupo == "total")

  inds_amenazadas_nacional <- default_indicadores("inds_amenazadas_nacional")
  inds_amenazadas_global <- default_indicadores("inds_amenazadas_global")

  inds_parent_amenazadas_nacional <- default_indicadores("parent_inds_amenazadas_nacional")
  inds_parent_amenazadas_global <- default_indicadores("parent_inds_amenazadas_global")

  inds_especies_parent_total <- default_indicadores("inds_especies_parent_total")



  ## Amenazadas - Amenazadas Nacional
  esp_list_nacional <- list_species(region, tematica = "amenazadas-nacional", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)
  ## Amenazadas - Amenazadas Global
  esp_list_global <- list_species(region, tematica = "amenazadas-global", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)


  # Amenazadas
  amenazadas <- list(
    slug = "amenazadas",
    label = "Amenazadas",
    children = list(
      c(list("slug" = "amenazadas-nacional", "label" = "Amenazadas nacional"),
        region_indicadores(region, inds_amenazadas_nacional, con = con),
        list(species_list = esp_list_nacional)),
      c(list("slug" = "amenazadas-global", "label" = "Amenazadas global"),
        region_indicadores(region, inds_amenazadas_global, con = con),
        list(species_list = esp_list_global))
    )
  )


  ## Amenazadas - Amenazadas Nacional
  amenazadas_nacional <- c(
    list(slug = "amenazadas-nacional", label = "Amenazadas nacional",
         parent_cr_estimadas = estimadas$especies_amenazadas_nacional_cr_estimadas,
         parent_en_estimadas = estimadas$especies_amenazadas_nacional_en_estimadas,
         parent_vu_estimadas = estimadas$especies_amenazadas_nacional_vu_estimadas
    ),
    region_indicadores(region, inds_amenazadas_nacional, con = con),
    region_indicadores(parent_region, inds_parent_amenazadas_nacional, con = con),
    region_indicadores(parent_region, inds_especies_parent_total, con = con),
    list(list_especies_amenazadas_nacional = NULL,
         list_especies_amenazadas_nacional_vu = NULL,
         list_especies_amenazadas_nacional_en = NULL,
         list_especies_amenazadas_nacional_cr = NULL),
    list(species_list = esp_list_nacional)
  )

  ## Amenazadas - Amenazadas Global

  amenazadas_global <- c(
    list(slug = "amenazadas-global", label = "Amenazadas global",
         cr_estimadas = estimadas$especies_amenazadas_global_cr_estimadas,
         en_estimadas = estimadas$especies_amenazadas_global_en_estimadas,
         vu_estimadas = estimadas$especies_amenazadas_global_vu_estimadas),
    region_indicadores(region, inds_amenazadas_global, con = con),
    region_indicadores(parent_region, inds_parent_amenazadas_global, con = con),
    region_indicadores(parent_region, inds_especies_parent_total, con = con),
    list(list_especies_amenazadas_global = NULL,
         list_especies_amenazadas_global_vu = NULL,
         list_especies_amenazadas_global_en = NULL,
         list_especies_amenazadas_global_cr = NULL),
    list(species_list = esp_list_global)
  )

  # CITES
  inds_cites <- default_indicadores("inds_cites")
  inds_parent_cites <- default_indicadores("inds_parent_cites")

  esp_list_cites <- list_species(region, tematica = "cites", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)

  esp_list_cites_i <- list_species(region, tematica = "cites-i", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)
  esp_list_cites_ii <- list_species(region, tematica = "cites-ii", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)
  esp_list_cites_i_ii <- list_species(region, tematica = "cites-i_ii", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)
  esp_list_cites_iii <- list_species(region, tematica = "cites-iii", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)

  cites <- c(
    list(slug = "cites",
         cites_total_estimadas = estimadas$especies_cites_total_estimadas,
         cites_i_estimadas = estimadas$especies_cites_i_estimadas,
         cites_i_ii_estimadas = estimadas$especies_cites_i_ii_estimadas,
         cites_ii_estimadas = estimadas$especies_cites_ii_estimadas,
         cites_iii_estimadas = estimadas$especies_cites_iii_estimadas),
    region_indicadores(region, inds_cites, con = con),
    region_indicadores(parent_region, inds_parent_cites, con = con),
    list(list_especies_cites = esp_list_cites,
         list_especies_cites_i = esp_list_cites_i,
         list_especies_cites_i_ii = esp_list_cites_i_ii,
         list_especies_cites_ii = esp_list_cites_ii,
         list_especies_cites_iii = esp_list_cites_iii),
    list(species_list = esp_list_cites)
  )

  # Endémicas
  inds_endemicas <- default_indicadores("inds_endemicas")
  inds_parent_endemicas <- default_indicadores("inds_parent_endemicas")

  esp_list_endemicas <- list_species(region, tematica = "endemicas", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)

  endemicas <- c(
    list(slug = "endemicas",
         endemicas_estimadas = estimadas$especies_endemicas_estimadas),
    region_indicadores(region, inds_endemicas, con = con),
    region_indicadores(parent_region, inds_parent_endemicas, con = con),
    list(list_especies_endemicas = NULL),
    list(species_list = esp_list_endemicas)
  )


  # Migratorias
  inds_migratorias <- default_indicadores("inds_migratorias")
  inds_parent_migratorias <- default_indicadores("inds_parent_migratorias")

  esp_list_migratorias <- list_species(region, tematica = "migratorias", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)

  migratorias <- c(
    list(slug = "migratorias",
         texto = "La información de especies migratorias se basa en la
         [Lista de referencia de especies de aves de Colombia - 2020](https://doi.org/10.15472/qhsz0p).
         Para aportar más datos sobre este y otros grupos bilógicos puede visitar nuestro sitio web
         https://biodiversidad.co",
         migratorias_estimadas = estimadas$especies_migratorias_estimadas),
    region_indicadores(region, inds_migratorias, con = con),
    region_indicadores(parent_region, inds_parent_migratorias, con = con),
    list(species_list = esp_list_migratorias)
  )


  # Exóticas
  inds_exoticas <- default_indicadores("inds_exoticas")
  inds_parent_exoticas <- default_indicadores("inds_parent_exoticas")

  ## OJOOOOOOO REVISAR EL DATO
  esp_list_exoticas_total <- list_species(region, tematica = "exoticas", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)
  esp_list_exoticas <- list_species(region, tematica = "exoticas", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)
  esp_list_invasoras <- list_species(region, tematica = "invasoras", con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)
  esp_list_exoticas_riesgo_invasion <- list_species(region,
                                                    tematica = "exotica-riesgo-invasion",
                                                    con = con) |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    distinct() |>
    slice(1:10)

  exoticas <- c(
    list(slug = "exoticas-invasoras",
         exoticas_total_estimadas = estimadas$especies_exoticas_total_estimadas,
         exoticas_estimadas = estimadas$especies_exoticas_estimadas,
         exoticas_riesgo_invasion_estimadas = estimadas$especies_exoticas_riesgo_invasion_estimadas,
         exoticas_invasoras_estimadas = estimadas$especies_invasoras_estimadas),
    region_indicadores(region, inds_exoticas, con = con),
    region_indicadores(parent_region, inds_parent_exoticas, con = con),
    list(list_especies_exoticas_total = esp_list_exoticas_total,
         list_especies_exoticas = esp_list_exoticas,
         list_especies_invasoras = esp_list_invasoras,
         list_especies_exoticas_riesgo_invasion = esp_list_exoticas_riesgo_invasion)
  )

  list(
    amenazadas,
    amenazadas_nacional,
    amenazadas_global,
    cites,
    endemicas,
    migratorias,
    exoticas
  )

}



