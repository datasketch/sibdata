test_that("List species work", {



  con <- DBI::dbConnect(RSQLite::SQLite(),
                        sys_file_sibdata("db/sibdata.sqlite"),
                        #"sibdata.sqlite",
                        read_only = TRUE)


  # Colombia Amenazadas

  region <- "colombia"

  # Amenazadas nacional
  inds_amenazadas_nacional <- default_indicadores("inds_amenazadas_nacional")
  reg_inds <- region_indicadores(region, inds_amenazadas_nacional, con = con)
  esp_list_nal <- list_species(region, tematica = "amenazadas-nacional", con = con) |>
    collect()

  unique_tematica <- c("amenazadas-nacional-vu",
                       "amenazadas-nacional-en","amenazadas-nacional-cr")
  expect_true(all(unique(esp_list_nal$slug_tematica) %in% unique_tematica))

  # Amenazadas glogabl
  inds_amenazadas_global <- default_indicadores("inds_amenazadas_global")
  reg_inds <- region_indicadores(region, inds_amenazadas_global, con = con)
  esp_list_glo <- list_species(region, tematica = "amenazadas-global", con = con) |>
    collect()

  unique_tematica <- c("amenazadas-global-vu",
                       "amenazadas-global-en","amenazadas-global-cr")
  expect_true(all(unique(esp_list_glo$slug_tematica) %in% unique_tematica))




  ## Colombia, Grupos, cifra
  sel_tipo <- "biologico"
  reg_gr_bio <- sibdata_region_grupo(con) |>
    collect() |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(tipo == sel_tipo) |>
    dplyr::mutate(slug = slug_grupo) |>
    dplyr::relocate(slug) |>
    distinct(slug, .keep_all = TRUE)

  grupo <- "hongos"
  tematica <- "amenazadas-global"


  x <- reg_gr_bio |> filter(slug == grupo)
  hongos_amenaza_global <- x$especies_amenazadas_global_total

  lesp <- list_species(region, grupo = grupo, tematica = tematica, con = con) |>
    collect()

  expect_equal(hongos_amenaza_global, nrow(lesp))



})


test_that("Exoticas", {

  region <- "colombia"

  especies <- sibdata_especie_tematica(con = con) |> collect()
  unique(especies$slug_tematica)


  exoticas_total <- list_species(region, tematica = "exoticas-total", con = con) |>
    collect()
  nrow(exoticas_total)

  # Colombia, animales, exóticas

  sel_tipo <- "biologico"
  reg_gr_bio <- sibdata_region_grupo(con) |>
    collect() |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(tipo == sel_tipo) |>
    dplyr::mutate(slug = slug_grupo) |>
    dplyr::relocate(slug) |>
    distinct(slug, .keep_all = TRUE)

  grupo <- "animales"
  tematica <- "exoticas-total"
  x <- reg_gr_bio |> filter(slug == grupo)
  animales_exoticas_total <- x$especies_exoticas_total
  lesp <- list_species(region, grupo = grupo, tematica = tematica, con = con) |>
    collect()

  expect_equal(x$especies_exoticas_total,   nrow(lesp))



})


test_that("especies tolima", {

  region <- "tolima"
  sel_tipo <- "biologico"
  tematica <- "cites"

  tolima_cites <- list_species(region, tematica = "cites", con = con) |>
    collect()



})






