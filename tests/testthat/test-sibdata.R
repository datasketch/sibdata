test_that("sibdata works", {

  # # Check params
  #
  # expect_error(check_cases_values("tipo", "notespecies"))
  # expect_error(check_cases_values("tipo", "registross"))
  #
  #
  # # Select indicators
  # tipo = NULL
  # cobertura = NULL
  # tematica = NULL
  #
  # sel_inds <- select_indicator()
  # expect_equal(length(sel_inds), nrow(sibdata_ind_meta()))
  #
  # sel_inds <- select_indicator(tipo = "especies")
  # sel_inds <- select_indicator(tipo = "especies", cobertura = "marinas")
  # sel_inds <- select_indicator(tipo = "registros", cobertura = "continentales",
  #                              tematica = "cites")
  #
  #
  # ###
  #
  # tipo = NULL
  # cobertura = NULL
  # tematica = NULL
  # grupo = NULL
  # subregiones = FALSE
  # with_parent = FALSE
  #
  #
  # region <- "tolima"
  # x1 <- sibdata("tolima")
  # expect_equal(names(x1), c("indicador", "count"))
  # expect_equal(x1$indicador, c("especies_region_total", "registros_region_total"))
  #
  # x2 <- sibdata("tolima", tipo = "registros")
  # expect_equal(names(x2), c("indicador", "count"))
  # expect_equal(x2$indicador,"registros_region_total")
  #
  # x3 <- sibdata("tolima", cobertura = "continentales")
  # expect_equal(names(x3), c("indicador", "count"))
  # expect_equal(x3$indicador,  c("especies_continentales", "registros_continentales"))
  #
  # x4 <- sibdata("tolima", tipo = "especies", cobertura = "marinas")
  # expect_equal(names(x4), c("indicador", "count"))
  # expect_equal(x4$indicador,  c("especies_marinas"))
  #
  # # Region con parent y subregiones
  #
  # y1 <- sibdata("tolima", with_parent = TRUE)
  # expect_equal(nrow(y1), 4)
  # y2 <- sibdata("tolima", tipo = "registros", subregiones = TRUE)
  # expect_equal(nrow(y2), 47)
  # expect_equal(unique(y2$indicador), "registros_region_total")
  #
  # # Region con tematica
  #
  # t1 <- sibdata("colombia", tematica = "amenazadas_nacional")
  #
  # t1 <- sibdata("colombia", tematica = "amenazadas_global")
  # expect_true(all(grepl("_cr|_en|_vu",t1$indicador)))
  #
  # t2 <- sibdata("colombia", tipo = "registros",
  #               tematica = "amenazadas_nacional")
  # expect_equal(t2$indicador,  c("registros_amenazadas_nacional_cr",
  #                               "registros_amenazadas_nacional_en",
  #                               "registros_amenazadas_nacional_vu"))
  # t3 <- sibdata("colombia", tipo = "especies",
  #               tematica = "cites")
  # expect_equal(t3$indicador,  c("especies_cites_i",
  #                               "especies_cites_ii",
  #                               "especies_cites_i_ii",
  #                               "especies_cites_iii"))
  #
  #
  # t4 <- sibdata("colombia", cobertura = "continentales",
  #               tipo = "especies",
  #               tematica = "endemicas")
  # expect_true(all(c("indicador", "count") %in% names(t4)))
  #
  # # Region con grupo biologico
  #
  # z1 <- sibdata("colombia", grupo = "abejas")
  # expect_equal(z1$indicador, c("especies_region_total", "registros_region_total"))
  #
  # z1 |> sib_merge_ind_label()
  #
  # z2 <- sibdata("colombia", grupo = "abejas",
  #               cobertura = "continentales")
  #
  #
  # # Region, Grupo, Temática
  #
  # region = "tolima"
  # tipo = "especies"
  # cobertura = NULL
  # tematica = "cites"
  # grupo = "animales"
  # subregiones = FALSE
  # with_parent = FALSE
  # all_indicators = FALSE
  #
  #
  # z2a <- sibdata("tolima", grupo = "animales", tipo = "especies",
  #               tematica = "cites")
  # z2 <- sibdata("tolima", grupo = "animales", tipo = "especies",
  #               tematica = "cites", all_indicators = TRUE)
  # z2b <- z2 |> filter(grepl("especies_cites", indicador)) |>
  #   filter(!grepl("total", indicador)) |>
  #   select(indicador, count)
  # expect_equal(z2a, z2b)
  #
  #
  #
  #
  # x <- sibdata("tolima")
  #
  # # Only Tipo
  # tipo <- "especies"
  # d <- sibdata_wide("tolima", tipo = tipo)
  # names(d)
  #
  # dd <- sibdata_tidify(d)
  #
  # d <- sibdata_wide("tolima", tipo = tipo, with_parent = TRUE)
  # d <- sibdata_wide("tolima", tipo = tipo, subregiones = TRUE)


})
