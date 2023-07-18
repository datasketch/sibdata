test_that("List species", {


  con <- DBI::dbConnect(RSQLite::SQLite(),
                        sys_file_sibdata("db/sibdata.sqlite"),
                        #"sibdata.sqlite",
                        read_only = TRUE)


  # Especies no repetidas en exoticas colombia

  esps_col <- list_species(region = "colombia",
                              tematica = "exoticas",
                              con = con) |>
    collect()

  esps_col_cites_i <- list_species(region = "colombia",
                           tematica = "cites-i",
                           con = con) |>
    collect()

  esps_col_cites <- list_species(region = "colombia",
                                   tematica = "cites",
                                   con = con) |>
    collect()


  esps_col <- list_species(region = "carmen-de-apicala",
                           tematica = "exoticas",
                           con = con) |>
    collect()


  #

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


  ### OJO CON EXÓTICAS TOTAL





})
