test_that("sibdata works", {

  tipo = NULL
  cobertura = NULL
  tematica = NULL
  grupo_biologico = NULL
  grupo_interes = NULL
  subregiones = NULL
  parent = NULL

  x <- sibdata("tolima")

  tipo <- "especies"
  x <- sibdata("tolima", tipo = tipo)
  tipo <- "todas"
  x <- sibdata("tolima", tipo = tipo)
  tipo <- "registros"
  x <- sibdata("tolima", tipo = tipo)

  tipo <- "registros"
  x <- sibdata("tolima", tipo = tipo, subregiones = TRUE)


  tipo = NULL
  cobertura = "continental"
  subregiones = NULL



})
