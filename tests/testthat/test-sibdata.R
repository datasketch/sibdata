test_that("sibdata works", {

  # Check params

  con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                        read_only = TRUE)

  expect_error(check_cases_values("tipo", "notespecies"))
  expect_error(check_cases_values("tipo", "registross"))


  # Select indicators
  tipo = NULL
  cobertura = NULL
  tematica = NULL

  sel_inds <- select_indicator(con = con)
  expect_equal(length(sel_inds), nrow(sibdata_indicadores(con) ))

  sel_inds <- select_indicator(tipo = "especies", con = con)
  sel_inds <- select_indicator(tipo = "especies", cobertura = "marinas", con = con)
  sel_inds <- select_indicator(tipo = "registros", cobertura = "continentales",
                               tematica = "cites", con = con)


  ###

  tipo = NULL
  cobertura = NULL
  tematica = NULL
  grupo = NULL
  subregiones = FALSE
  with_parent = FALSE


  region <- "tolima"
  x1 <- sibdata("tolima", con = con)
  expect_equal(names(x1), c("indicador", "count"))
  expect_equal(x1$indicador, c("especies_region_total",
                               "especies_region_estimadas",
                               "registros_region_total"))

  x2 <- sibdata("tolima", tipo = "registros", con = con)
  expect_equal(names(x2), c("indicador", "count"))
  expect_equal(x2$indicador,"registros_region_total")

  x3 <- sibdata("tolima", cobertura = "continentales", con = con)
  expect_equal(names(x3), c("indicador", "count"))
  expect_equal(x3$indicador,  c("especies_continentales", "registros_continentales"))

  x4 <- sibdata("tolima", tipo = "especies", cobertura = "marinas", con = con)
  expect_equal(names(x4), c("indicador", "count"))
  expect_equal(x4$indicador,  c("especies_marinas"))

  # Region con parent y subregiones

  y1 <- sibdata("tolima", with_parent = TRUE, con = con)
  expect_equal(nrow(y1), 6)
  y2 <- sibdata("tolima", tipo = "registros", subregiones = TRUE, con = con )
  expect_equal(nrow(y2), 47)
  expect_equal(unique(y2$indicador), "registros_region_total")

  # Region con tematica

  t1 <- sibdata("colombia", tematica = "amenazadas_nacional", con = con)

  t1 <- sibdata("colombia", tematica = "amenazadas_global", con = con)
  expect_true(all(grepl("_cr|_en|_vu",t1$indicador)))

  t2 <- sibdata("colombia", tipo = "registros",
                tematica = "amenazadas_nacional", con = con)
  expect_equal(t2$indicador,  c("registros_amenazadas_nacional_cr",
                                "registros_amenazadas_nacional_en",
                                "registros_amenazadas_nacional_vu"))
  t3 <- sibdata("colombia", tipo = "especies",
                tematica = "cites", con = con)
  expect_equal(t3$indicador,  c("especies_cites_i",
                                "especies_cites_ii",
                                "especies_cites_i_ii",
                                "especies_cites_iii"))

  sibdata("colombia", indicador = "especies_cites_total",
          subregiones = TRUE,
          con = con)

  d <- sibdata("colombia",
          tipo = "registros",
          tematica = "amenazadas_nacional",
          indicador = "registros_amenazadas_nacional_en",
          subregiones = TRUE,
          con = con)
  sib_merge_ind_label(d, con = con)

  sibdata("colombia", indicador = "especies_exoticas_total",
          subregiones = TRUE,
          con = con)

  t4 <- sibdata("colombia", cobertura = "continentales",
                tipo = "especies",
                tematica = "endemicas", con = con)
  expect_true(all(c("indicador", "count") %in% names(t4)))


  sibdata("colombia", tematica = "exoticas_total",
          subregiones = TRUE,
          con = con)

  sibdata("colombia", tematica = "exoticas",
          subregiones = TRUE,
          con = con)

  sibdata("colombia", tematica = "exoticas_total",
          indicador = "especies_exoticas_total",
          subregiones = TRUE,
          con = con)

  sibdata("colombia", indicador = "especies_exoticas_total",
          subregiones = TRUE,
          con = con)
  sibdata("colombia", tematica = "cites",
          subregiones = TRUE,
          con = con)

  # Region con grupo biologico

  z1 <- sibdata("colombia", grupo = "abejas", con = con)
  expect_equal(z1$indicador, c("especies_region_total", "registros_region_total"))

  z1 |> sib_merge_ind_label(con = con)

  z2 <- sibdata("colombia", grupo = "abejas",
                cobertura = "continentales", con = con)


  # Region, Grupo, Temática

  region = "tolima"
  tipo = "especies"
  cobertura = NULL
  tematica = "cites"
  grupo = "animales"
  subregiones = FALSE
  with_parent = FALSE
  all_indicators = FALSE


  z2a <- sibdata("tolima", grupo = "animales", tipo = "especies",
                tematica = "cites", con = con)
  z2 <- sibdata("tolima", grupo = "animales", tipo = "especies",
                tematica = "cites", all_indicators = TRUE, con = con)
  z2b <- z2 |> filter(grepl("especies_cites", indicador)) |>
    filter(!grepl("total", indicador)) |>
    select(indicador, count)
  expect_equal(z2a, z2b)




  x <- sibdata("tolima", con = con)

  # Only Tipo
  tipo <- "especies"
  d <- sibdata_wide("tolima", tipo = tipo, con = con)
  names(d)

  dd <- sibdata_tidify(d, con = con)

  d <- sibdata_wide("tolima", tipo = tipo, with_parent = TRUE, con = con)
  d <- sibdata_wide("tolima", tipo = tipo, subregiones = TRUE, con = con)


  ###

  t2 <- sibdata("colombia", tipo = "especies",
                tematica = NULL, con = con)


  sibdata("colombia", tipo = "especies",
          tematica = 'OTRO', indicador = "registros_invasoras", con = con)

  ##

  sibdata(region = "colombia", tipo = "especies", grupo = "animales",
          tematica = 'endemicas',
          subregiones = TRUE, con = con)


})
