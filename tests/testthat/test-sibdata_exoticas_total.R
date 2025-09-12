test_that("multiplication works", {

    con <- DBI::dbConnect(RSQLite::SQLite(),
                          sys_file_sibdata("db/sibdata.sqlite"),
                          read_only = TRUE)
    on.exit(DBI::dbDisconnect(con))

    # sel_region: colombia
    # sel_region_tipo: Nacional
    # sel_grupo_tipo: biologico
    # sel_grupo:
    #   sel_tematica: exoticas-total
    # sel_tipo: registros
    # tematica: exoticas_total


    region <- "colombia"
    tematica <- "exoticas_total"
    tipo <- "registros"
    indicador <- NULL
    #indicador <- "registros_invasoras"
    x1 <- sibdata(region = region,
                  tipo = tipo,
                  tematica = tematica,
                  indicador = indicador,
                  #subregiones =  TRUE,
                  #with_parent = FALSE,
                  con = con)
    expect_true(nrow(x1) == 1)

    indicador <- "registros_exotica_riesgo_invasion_total"
    x1 <- sibdata(region = region,
                  indicador = indicador,
                  con = con)

    # FUNCTION INPUTS:
    #   - region: colombia
    # - tipo: registros
    # - tematica: exoticas-total
    # - indicador: registros_exotica_riesgo_invasion_total
    # - grupo:
    #   - subregiones: TRUE
    # - with_parent: FALSE

    region <- "colombia"
    tipo <- "registros"
    tematica <- "exoticas_total"
    indicador <- "registros_exotica_riesgo_invasion_total"
    x1 <- sibdata(region = region,
                  tipo = tipo,
                  tematica = tematica,
                  indicador = indicador,
                  subregiones =  TRUE,
                  with_parent = FALSE,
                  con = con)



    # Exóticas total
    region <- "colombia"
    # tematica <- "cites"
    tematica <- "amenazadas_global"
    tipo <- "registros"
    # indicador <- "especies_cites_total"
    indicador <- "especies_amenazadas_global_total"
    x1 <- sibdata(region = region,
                  tipo = tipo,
                  tematica = tematica,
                  indicador = indicador,
                  subregiones =  TRUE,
                  with_parent = FALSE,
                  con = con)
    expect_true(nrow(x1) == 33)



    expect_equal(names(x1), c("indicador", "count"))
    expect_equal(x1$indicador, c("especies_region_total",
                                 "especies_region_estimadas",
                                 "registros_region_total"))



    region <- "tolima"
    tematica <- "invasoras"
    x1 <- sibdata("tolima", con = con)

    region <- "tolima"
    tematica <- "invasoras"
    indicador <- "registros_invasoras"
    x1 <- sibdata(region = region,
                  indicador = indicador,
                  con = con)

    indicador <- "registros_exoticas_riesgo_invasion_total"
    x1 <- sibdata(region = region,
                  indicador = indicador,
                  con = con)

})
