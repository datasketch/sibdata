test_that("multiplication works", {


  expect_equal(sib_available_regions(subtipo = "País"),
               c("Colombia"  = "colombia"))

  deptos <- sib_available_regions(subtipo = "Departamento")
  expect_equal(names(deptos), c("Boyacá", "Nariño", "Santander", "Tolima"))

})
