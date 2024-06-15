test_that("multiplication works", {

  #dbdir <- sys_file_sibdata("db/sibdata.duckdb")
  con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                        read_only = TRUE)

  ind <- sibdata_indicadores(con)

  input <- list(
    region = "colombia",
    tipo = "registros",
    subregiones = TRUE,
    con = con
  )
  d <- do.call("sibdata", input)
  choropleth_map(d = d,
                 con = con,
                 region = input$region)

  input <- list(
    region = "colombia",
    tipo = "especies",
    subregiones = TRUE,
    con = con
  )
  d <- do.call("sibdata", input)
  choropleth_map(d = d,
                 con = con,
                 region = input$region)

  ###
  inp <- list(
    region = "colombia",
    tipo = "especies",
    indicador = "especies_region_total",
    subregiones = TRUE,
    con = con
  )
  d <- do.call("sibdata", inp)
  choropleth_map(d = d,
                 con = con,
                 region = inp$region,
                 indicador = inp$indicador)


  input <- list(
    region = "colombia",
    grupo = "animales",
    tipo = "especies",
    tematica = "endemicas",
    subregiones = TRUE,
    with_parent = FALSE,
    con = con
  )
  inp <- input

  d <- do.call("sibdata", input)

  choropleth_map(d = d,
    region = inp$region,
    grupo = inp$grupo,
    tipo = inp$tipo,
    cobertura = inp$cobertura,
    tematica = inp$tematica,
    con = con
  )


  input <- list(
    region = "colombia",
    # grupo = "aracnidos",
    tipo = "registros",
    tematica = "amenazadas_nacional",
    subregiones = TRUE,
    con = con
  )
  inp <- input
  d <- do.call("sibdata", input)
  choropleth_map(d = d,
                 tematica = inp$tematica,
                 con = con,
                 region = inp$region)



  input <- list(
    region = "colombia",
    # grupo = "aracnidos",
    # tipo = "especies",
    # tematica = "amenazadas_nacional",
    indicador = "especies_amenazadas_nacional_vu",
    subregiones = TRUE,
    con = con
  )
  inp <- input
  d <- do.call("sibdata", input)
  choropleth_map(d = d,
                 con = con,
                 region = input$region,
                 indicador = input$indicador)



  # $ region     : chr "colombia"
  # $ grupo      : chr "aracnidos"
  # $ tipo       : chr "especies"
  # $ tematica   : chr "migratorias"
  input <- list(
    region = "colombia",
    grupo = "aracnidos",
    tipo = "registros",
    tematica = "migratorias",
    subregiones = TRUE,
    con = con
  )
  d <- do.call("sibdata", input)
  choropleth_map(d = d,
                 con = con,
                 region = input$region)




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
