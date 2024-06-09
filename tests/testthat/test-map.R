test_that("multiplication works", {

  #dbdir <- sys_file_sibdata("db/sibdata.duckdb")
  con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                        read_only = TRUE)
  input <- list(
    region = "colombia",
    grupo = "animales",
    tipo = "especies",
    tematica = "endemicas",
    subregiones = FALSE,
    with_parent = FALSE
  )
  inp <- input
  region <- inp$region
  choropleth_map(
    inp$region,
    grupo = inp$grupo,
    tipo = inp$tipo,
    cobertura = inp$cobertura,
    tematica = inp$tematica,
    con = con
  )


  input <- list(
    region = "boyaca",
    grupo = "animales",
    tipo = "especies",
    tematica = "endemicas",
    subregiones = FALSE,
    with_parent = FALSE
  )
  inp <- input
  region <- inp$region
  choropleth_map(
    inp$region,
    grupo = inp$grupo,
    tipo = inp$tipo,
    cobertura = inp$cobertura,
    tematica = inp$tematica,
    con = con
  )

  # Colombia

  input <- list(
    region = "colombia",
    grupo = NULL,
    tipo = "registros",
    tematica = NULL,
    subregiones = FALSE,
    with_parent = FALSE,
    con = con
  )
  inp <- input
  region <- inp$region
  choropleth_map(
    region = inp$region,
    grupo = inp$grupo,
    tipo = inp$tipo,
    cobertura = inp$cobertura,
    tematica = inp$tematica,
    con = inp$con
  )
  opts <- inp
  viz <- "sibdata::choropleth_map"
  viz <- "choropleth_map"
  do.call(eval(parse(text=viz)), opts)


  # Colombia - Amenazadas Nacional

  # Colombia

  input <- list(
    region = "colombia",
    grupo = NULL,
    tipo = "registros",
    tematica = "amenazadas_nacional",
    indicador = "registros_amenazadas_nacional_cr",
    subregiones = FALSE,
    with_parent = FALSE,
    con = con
  )
  inp <- input
  region <- inp$region
  choropleth_map(
    region = inp$region,
    grupo = inp$grupo,
    tipo = inp$tipo,
    cobertura = inp$cobertura,
    indicador = inp$indicador,
    tematica = inp$tematica,
    con = inp$con
  )



  input <- list(
    region = "colombia",
    grupo = NULL,
    tipo = "registros",
    #tematica = "amenazadas_nacional",
    indicador = "registros_exoticas_riesgo_invasion",
    subregiones = FALSE,
    with_parent = FALSE,
    con = con
  )
  inp <- input
  region <- inp$region
  choropleth_map(
    region = inp$region,
    grupo = inp$grupo,
    tipo = inp$tipo,
    cobertura = inp$cobertura,
    indicador = inp$indicador,
    tematica = inp$tematica,
    con = inp$con
  )


})
