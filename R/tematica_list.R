
tematica_list <- function(region){

  # region <- "colombia"
  # region <- "boyaca"
  #reg_tematica <- region_tematica(region)

  # esp_label <- especie_label()
  # esp_reg <- especie_region(region)
  # esp_reg_tem <- especie_region_tematica(region)
  # tem <- sib_tables("tematica") #|>
  #region_tem_long <- region_tematica_long(region)

  parent_region <- sib_parent_region(region)

  inds_amenazadas_nacional <- default_indicadores("inds_amenazadas_nacional")
  inds_amenazadas_global <- default_indicadores("inds_amenazadas_global")
  inds_especies_parent <- default_indicadores("inds_especies_parent")

  esp_list <- list_species(region, tematica = "amenazadas") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  # Amenazadas
  amenazadas <- list(
    slug = "amenazadas",
    label = "Amenazadas",
    children = list(
      c(list("slug" = "amenazadas-nacional", "label" = "Amenazadas nacional"),
        region_indicadores(region, inds_amenazadas_nacional)),
      c(list("slug" = "amenazadas-global", "label" = "Amenazadas global"),
        region_indicadores(region, inds_amenazadas_global))
    ),
    species_list = esp_list
  )


  esp_list <- list_species(region, tematica = "amenazadas-nacional") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  ## Amenazadas - Amenazadas Nacional
  amenazadas_nacional <- c(
    list(slug = "amenazadas-nacional", label = "Amenazadas nacional"),
    region_indicadores(region, inds_amenazadas_nacional),
    region_indicadores(parent_region, inds_especies_parent),
    list(list_especies_amenazadas_nacional = NULL,
         list_especies_amenazadas_nacional_vu = NULL,
         list_especies_amenazadas_nacional_en = NULL,
         list_especies_amenazadas_nacional_cr = NULL),
    list(species_list = esp_list)
  )

  ## Amenazadas - Amenazadas Global

  esp_list <- list_species(region, tematica = "amenazadas-global") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  amenazadas_global <- c(
    list(slug = "amenazadas-global", label = "Amenazadas global"),
    region_indicadores(region, inds_amenazadas_global),
    region_indicadores(region, inds_especies_parent),
    list(list_especies_amenazadas_global = NULL,
         list_especies_amenazadas_global_vu = NULL,
         list_especies_amenazadas_global_en = NULL,
         list_especies_amenazadas_global_cr = NULL),
    list(species_list = esp_list)
  )

  # CITES
  inds_cites <- default_indicadores("inds_cites")
  inds_parent_cites <- default_indicadores("inds_parent_cites")

  esp_list_cites <- list_species(region, tematica = "cites") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  esp_list_cites_i <- list_species(region, tematica = "cites-i") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_cites_ii <- list_species(region, tematica = "cites-ii") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_cites_i_ii <- list_species(region, tematica = "cites-i_ii") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_cites_iii <- list_species(region, tematica = "cites-iii") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  cites <- c(
    list(slug = "cites"),
    region_indicadores(region, inds_cites),
    region_indicadores(parent_region, inds_parent_cites),
    list(list_especies_cites = esp_list_cites,
         list_especies_cites_i = esp_list_cites_i,
         list_especies_cites_i_ii = esp_list_cites_i_ii,
         list_especies_cites_ii = esp_list_cites_ii,
         list_especies_cites_iii = esp_list_cites_iii),
    list(species_list = esp_list)
  )

  # Endémicas
  inds_endemicas <- default_indicadores("inds_endemicas")
  inds_parent_endemicas <- default_indicadores("inds_parent_endemicas")

  esp_list_endemicas <- list_species(region, tematica = "endemicas") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  endemicas <- c(
    list(slug = "endemicas"),
    region_indicadores(region, inds_endemicas),
    region_indicadores(parent_region, inds_parent_endemicas),
    list(list_especies_endemicas = NULL),
    list(species_list = esp_list_endemicas)
  )


  # Migratorias
  inds_migratorias <- default_indicadores("inds_migratorias")
  inds_parent_migratorias <- default_indicadores("inds_parent_migratorias")

  esp_list_migratorias <- list_species(region, tematica = "migratorias") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  migratorias <- c(
    list(slug = "migratorias",
         texto = "Existen otras especies migratorias pero aún no están documentadas
         a través del SiB Colombia,"),
    region_indicadores(region, inds_migratorias),
    region_indicadores(parent_region, inds_parent_migratorias),
    list(species_list = esp_list_migratorias)
  )


  # Exóticas
  inds_exoticas <- default_indicadores("inds_exoticas")
  inds_parent_exoticas <- default_indicadores("inds_parent_exoticas")

  ## OJOOOOOOO REVISAR EL DATO
  esp_list_exoticas_total <- list_species(region, tematica = "exoticas") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_exoticas <- list_species(region, tematica = "exoticas") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_invasoras <- list_species(region, tematica = "invasoras") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_exoticas_riesgo_invasion <- list_species(region, tematica = "riesgo-invasion") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  exoticas <- c(
    list(slug = "exoticas-invasoras"),
    region_indicadores(region, inds_exoticas),
    region_indicadores(parent_region, inds_parent_exoticas),
    list(list_especies_exoticas_total = esp_list_exoticas_total,
         list_especies_exoticas = esp_list_exoticas,
         list_especies_invasoras = NULL,
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




tematica_list_col <- function(region){

  region <- "colombia"
  # region <- "boyaca"
  #reg_tematica <- region_tematica(region)

  # esp_label <- especie_label()
  # esp_reg <- especie_region(region)
  # esp_reg_tem <- especie_region_tematica(region)
  # tem <- sib_tables("tematica") #|>
  #region_tem_long <- region_tematica_long(region)

  parent_region <- sib_parent_region(region)

  estimadas <- sibdata_estimada() |> collect() |>
    filter(slug_grupo == "total")

  inds_amenazadas_nacional <- default_indicadores("inds_amenazadas_nacional")
  inds_amenazadas_global <- default_indicadores("inds_amenazadas_global")
  inds_especies_parent <- default_indicadores("inds_especies_parent")

  esp_list <- list_species(region, tematica = "amenazadas") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  # Amenazadas
  amenazadas <- list(
    slug = "amenazadas",
    label = "Amenazadas",
    children = list(
      c(list("slug" = "amenazadas-nacional", "label" = "Amenazadas nacional"),
        region_indicadores(region, inds_amenazadas_nacional)),
      c(list("slug" = "amenazadas-global", "label" = "Amenazadas global"),
        region_indicadores(region, inds_amenazadas_global))
    ),
    species_list = esp_list
  )


  esp_list <- list_species(region, tematica = "amenazadas-nacional") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  ## Amenazadas - Amenazadas Nacional
  amenazadas_nacional <- c(
    list(slug = "amenazadas-nacional", label = "Amenazadas nacional",
         estimadas_cr = estimadas$especies_amenazadas_nacional_cr_estimadas,
         estimadas_en = estimadas$especies_amenazadas_nacional_en_estimadas,
         estimadas_vu = estimadas$especies_amenazadas_nacional_vu_estimadas
         ),
    region_indicadores(region, inds_amenazadas_nacional),
    region_indicadores(parent_region, inds_especies_parent),
    list(list_especies_amenazadas_nacional = NULL,
         list_especies_amenazadas_nacional_vu = NULL,
         list_especies_amenazadas_nacional_en = NULL,
         list_especies_amenazadas_nacional_cr = NULL),
    list(species_list = esp_list)
  )

  ## Amenazadas - Amenazadas Global

  esp_list <- list_species(region, tematica = "amenazadas-global") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  amenazadas_global <- c(
    list(slug = "amenazadas-global", label = "Amenazadas global",
         estimadas_cr = estimadas$especies_amenazadas_global_cr_estimadas,
         estimadas_en = estimadas$especies_amenazadas_global_en_estimadas,
         estimadas_vu = estimadas$especies_amenazadas_global_vu_estimadas),
    region_indicadores(region, inds_amenazadas_global),
    region_indicadores(region, inds_especies_parent),
    list(list_especies_amenazadas_global = NULL,
         list_especies_amenazadas_global_vu = NULL,
         list_especies_amenazadas_global_en = NULL,
         list_especies_amenazadas_global_cr = NULL),
    list(species_list = esp_list)
  )

  # CITES
  inds_cites <- default_indicadores("inds_cites")
  inds_parent_cites <- default_indicadores("inds_parent_cites")

  esp_list_cites <- list_species(region, tematica = "cites") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  esp_list_cites_i <- list_species(region, tematica = "cites-i") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_cites_ii <- list_species(region, tematica = "cites-ii") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_cites_i_ii <- list_species(region, tematica = "cites-i_ii") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_cites_iii <- list_species(region, tematica = "cites-iii") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  cites <- c(
    list(slug = "cites",
         estimadas_cites_i = estimadas$especies_cites_i_estimadas,
         estimadas_cites_i_ii = estimadas$especies_cites_i_ii_estimadas,
         estimadas_cites_ii = estimadas$especies_cites_ii_estimadas,
         estimadas_cites_iii = estimadas$especies_cites_iii_estimadas),
    region_indicadores(region, inds_cites),
    region_indicadores(parent_region, inds_parent_cites),
    list(list_especies_cites = esp_list_cites,
         list_especies_cites_i = esp_list_cites_i,
         list_especies_cites_i_ii = esp_list_cites_i_ii,
         list_especies_cites_ii = esp_list_cites_ii,
         list_especies_cites_iii = esp_list_cites_iii),
    list(species_list = esp_list)
  )

  # Endémicas
  inds_endemicas <- default_indicadores("inds_endemicas")
  inds_parent_endemicas <- default_indicadores("inds_parent_endemicas")

  esp_list_endemicas <- list_species(region, tematica = "endemicas") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  endemicas <- c(
    list(slug = "endemicas",
         estimadas_endemicas = estimadas$especies_endemicas_estimadas),
    region_indicadores(region, inds_endemicas),
    region_indicadores(parent_region, inds_parent_endemicas),
    list(list_especies_endemicas = NULL),
    list(species_list = esp_list_endemicas)
  )


  # Migratorias
  inds_migratorias <- default_indicadores("inds_migratorias")
  inds_parent_migratorias <- default_indicadores("inds_parent_migratorias")

  esp_list_migratorias <- list_species(region, tematica = "migratorias") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  migratorias <- c(
    list(slug = "migratorias",
         texto = "Existen otras especies migratorias pero aún no están documentadas
         a través del SiB Colombia",
         estimadas_migratorias = estimadas$especies_migratorias_estimadas),
    region_indicadores(region, inds_migratorias),
    region_indicadores(parent_region, inds_parent_migratorias),
    list(species_list = esp_list_migratorias)
  )


  # Exóticas
  inds_exoticas <- default_indicadores("inds_exoticas")
  inds_parent_exoticas <- default_indicadores("inds_parent_exoticas")

  ## OJOOOOOOO REVISAR EL DATO
  esp_list_exoticas_total <- list_species(region, tematica = "exoticas") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_exoticas <- list_species(region, tematica = "exoticas") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_invasoras <- list_species(region, tematica = "invasoras") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)
  esp_list_exoticas_riesgo_invasion <- list_species(region, tematica = "riesgo-invasion") |>
    select(label, slug_especie, registros, url_gbif, url_cbc, slug_tematica) |>
    arrange(desc(registros)) |>
    collect() |>
    slice(1:10)

  exoticas <- c(
    list(slug = "exoticas-invasoras",
         estimadas_exoticas_total = estimadas$especies_exoticas_total_estimadas,
         estimadas_exoticas = estimadas$especies_exoticas_estimadas,
         estimadas_exoticas_riesgo_invasion = estimadas$especies_exoticas_riesgo_invasion_estimadas,
         estimadas_exoticas_invasoras = estimadas$especies_invasoras_estimadas),
    region_indicadores(region, inds_exoticas),
    region_indicadores(parent_region, inds_parent_exoticas),
    list(list_especies_exoticas_total = esp_list_exoticas_total,
         list_especies_exoticas = esp_list_exoticas,
         list_especies_invasoras = NULL,
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