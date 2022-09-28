test_that("List species", {

  esps_tolima <- list_species(region = "tolima") |>
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
