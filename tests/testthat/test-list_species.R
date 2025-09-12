test_that("List species", {


  con <- DBI::dbConnect(RSQLite::SQLite(),
                        sys_file_sibdata("db/sibdata.sqlite"),
                        #"sibdata.sqlite",
                        read_only = TRUE)

  # Cites colombia

  esps_col_cites <- list_species(region = "colombia",
                                 tematica = "cites",
                                 con = con) |>
    collect()
  expect_equal(nrow(esps_col_cites), 2897)


  esps_col_cites_i <- list_species(region = "colombia",
                           tematica = "cites-i",
                           con = con) |>
    collect()
  expect_equal(nrow(esps_col_cites_i), 48)

  esps_col_cites_i_ii <- list_species(region = "colombia",
                                   tematica = "cites-i-ii",
                                   con = con) |>
    collect()
  expect_equal(nrow(esps_col_cites_i_ii), 4)

  esps_col_cites_ii <- list_species(region = "colombia",
                                   tematica = "cites-ii",
                                   con = con) |>
    collect()
  expect_equal(nrow(esps_col_cites_ii), 2814)

  esps_col_cites_iii <- list_species(region = "colombia",
                                    tematica = "cites-iii",
                                    con = con) |>
    collect()
  expect_equal(nrow(esps_col_cites_iii), 31)

  # 48 + 4 + 2814 + 31 = 2897
  esps_col_cites_distinct <- bind_rows(list(
    esps_col_cites_i,
    esps_col_cites_i_ii,
    esps_col_cites_ii,
    esps_col_cites_iii
  )) |> distinct()
  expect_equal(nrow(esps_col_cites_distinct), 2897)



  # Exóticas colombia

  esps_col <- list_species(region = "colombia",
                           tematica = "exoticas-total",
                           con = con) |> collect()
  n <- sibdata(region = "colombia", indicador = "especies_exoticas_total",
               con = con)[[3]]
  expect_equal(n, 1244)


  esps_col <- list_species(region = "colombia",
                           tematica = "exoticas-riesgo-invasion-total",
                           con = con) |>
    collect()
  n <- sibdata(region = "colombia",
          indicador = "especies_exoticas_riesgo_invasion_total",
          con = con)[[3]]
  expect_equal(nrow(esps_col), n)

  # Tolima

  esps_tolima <- list_species(region = "tolima", con = con) |>
    collect()

  esps_tolima_amenazadas <- list_species(region = "tolima",
                                         tematica = "amenazada") |>
    collect()

  esps_tolima_aves <- list_species(region = "tolima",
                                         grupo = "aves") |>
    collect()

  esps_tolima_anfibios_amenazadas <- list_species(region = "tolima",
                                   grupo = "anfibios",
                                   tematica = "amenazadas-global") |>
    collect()


  esps_col <- list_species(region = "carmen-de-apicala",
                           tematica = "exoticas",
                           con = con) |>
    collect()





})
