test_that("multiplication works", {

  info <- info_pages()
  available_pages <- c("publicador", "preg_frecuentes", "glosario", "tooltips")
  expect_equal(names(info), available_pages)
})
