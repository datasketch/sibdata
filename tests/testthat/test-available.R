test_that("multiplication works", {

  con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                        read_only = TRUE)
  expect_equal(sib_available_regions(subtipo = "País", con = con),
               c("Colombia"  = "colombia"))

  deptos <- sib_available_regions(subtipo = "Departamento")
  expect_equal(names(deptos), c("Boyacá", "Nariño", "Santander", "Tolima"))

})
