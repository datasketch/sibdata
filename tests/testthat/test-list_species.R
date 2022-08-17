test_that("List species", {

  esps_tolima <- list_species(region = "tolima")

  esps_tolima_amenazadas <- list_species(region = "tolima",
                                         tematica = "amenazada")

  esps_tolima_aves <- list_species(region = "tolima",
                                         grupo_biologico = "aves")

  esps_tolima_anfibios_amenazadas <- list_species(region = "tolima",
                                   grupo_biologico = "anfibios",
                                   tematica = "amenazadas-global")


})
