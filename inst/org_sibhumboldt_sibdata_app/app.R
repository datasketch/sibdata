
library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)
library(hgmagic)
library(leaflet)
library(shinyinvoer)
library(dsmods)
library(dsmodules)
library(geotable)
library(sibdata)
library(duckdbits)
library(shinyjs)


debug <- TRUE
# debug <- FALSE

dbdir <- "db/sibdata.sqlite"
con <- DBI::dbConnect(RSQLite::SQLite(), dbdir, read_only = TRUE)


av_grupos_int <- sib_available_grupos(tipo = "interes", con = con)
opts_grupo_interes <-  c("Todos" = "todos", av_grupos_int)


paste_dash <- function(str, times = 1){
  paste(" ", paste0(rep("-",times-1), collapse = ""),str)
}

gru <- sibdata_grupo(con) |> collect() |> filter(tipo == "biologico")
gru_tree <- data.tree::FromDataFrameNetwork(gru)
gru_df <- data.tree::ToDataFrameNetwork(gru_tree,
                                        direction = "descend",
                                        "label", "level", "path")

opt_gru <- gru_df |>
  rowwise() |>
  mutate(label = paste_dash(label, level)) |>
  arrange(path)
opts_grupo_biologico <- opt_gru$from
names(opts_grupo_biologico) <- opt_gru$label
opts_grupo_biologico <- c("Todos" = "todos", opts_grupo_biologico)


pais <- sib_available_regions(subtipo = "País", con = con)
departamentos <- sib_available_regions(subtipo = "Departamento", con = con)
opts_region <- c(pais, sort(departamentos))
opts_region <- c(
  opts_region,
  "Resguardo Pialapí Pueblo Viejo" = "resguardo-indigena-pialapi-pueblo-viejo",
  "Reserva Natural La Planada" = "reserva-natural-la-planada"
)


opts_tematicas <- c(sib_available_tematicas(), "Ninguna" = "todas")
opts_tematicas_ex <- c("cites_i", "cites_ii","cites_i_ii", "cites_iii",
                       "exoticas_total"
                       #"exoticas", "invasoras", "riesgo_invasion"
)
opts_tematicas <- opts_tematicas[!opts_tematicas %in% opts_tematicas_ex]
# opts_tematicas <- gsub("_","-",opts_tematicas) # hay diferencia entre sibdata y list_species
# uno recibe _ y el otro -


# UI ###############

ui <- panelsPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),
  panel(title = "Opciones", width = 280,
        body = div(
          uiOutput("debug"),
          uiOutput("sel_region_"),
          hr(),
          radioButtons("sel_grupo_type", "Tipo de grupo",
                       c("Biológico" = "biologico", "Interés de Conservación" = "interes")),
          uiOutput("sel_grupo_opts"),
          hr(),
          #radioButtons("sel_cobertura", "Cobertura", c("Total" = "total","Continental" = "continentales","Marina" = "marinas")),
          uiOutput("sel_tematica_"),
          br()
        ),
        footer = ""),
  panel(title = "Gráficos",
        can_collapse = FALSE,
        header_right = div(style = "display: flex;",
                           div(
                             class='first-container',
                             uiOutput("viz_type")
                           ),
                           div(class='second-container',NULL)
        ),
        body = div(
          div(style = "display: flex; justify-content: space-between;",
              div(style = "flex: 1;",
                  radioButtons("sel_tipo", "Tipo",
                               c("Observaciones" = "registros",
                                 "Especies"="especies")
                  )
              ),
              div(style = "flex: 1;",uiOutput("data_controls"))
          ),
          hr(),
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(style = "flex: 3;",textOutput("breadcrumb")),
              div(style = "flex: 1;text-align: right;",uiOutput("descargas"))
          ),
          br(),
          uiOutput("viz"),
          uiOutput("debug_table"),
          br()
        ),
        footer = ""),
  panel(title = "Especies",
        width = 400,
        can_collapse = FALSE,
        header_right = downloadTableUI("species_table",
                                       dropdownLabel = "Descargar especies",
                                       formats = c("csv", "xlsx", "json"),
                                       display = "dropdown",
                                       dropdownWidth = 200),
        body = dataTableOutput("list_species")
  )
)

## SERVER ############

server <-  function(input, output, session) {

  conmap <- gt_con()

  r <- reactiveValues(
    amenazadas_categoria = NULL,
    cites_categoria = NULL,
    exotica_categoria = NULL,
    especies_total_estimadas = NULL,
    indicador = NULL,
    show_subcategoria = FALSE,
    show_especies_total_estimadas = FALSE
  )

  par <- list(region = NULL, tematica = NULL, grupo = NULL)
  url_par <- reactive({
    url_params(par, session)$inputs
  })

  output$debug <- renderUI({
    if(debug){
      list(
        verbatimTextOutput("debug1"),
        verbatimTextOutput("debug2")
      )
    }
  })

  output$debug_table <- renderUI({
    if(debug){
      dataTableOutput("data_viz")
    }
  })




  ### UI  #####

  output$sel_region_ <- renderUI({
    req(opts_region)
    default_select <- NULL
    if (!is.null(url_par()$region)) default_select <- tolower(url_par()$region)
    selectizeInput("sel_region","Seleccione Región",
                   opts_region,
                   selected = default_select
    )
  })

  output$sel_grupo_opts <- renderUI({
    req(input$sel_grupo_type)

    default_select <- NULL
    if (!is.null(url_par()$grupo)) default_select <- tolower(url_par()$grupo)
    opts <- opts_grupo_interes

    list(
      conditionalPanel(
        condition = "input.sel_grupo_type == 'biologico'",
        selectInput("sel_grupo_bio",
                    "Seleccione grupo biológico",
                    opts_grupo_biologico)
      ),
      conditionalPanel(
        condition = "input.sel_grupo_type != 'biologico'",
        selectInput("sel_grupo_int",
                    "Seleccione grupo de interés",
                    opts_grupo_interes)
      )
    )

  })

  sel_grupo <- reactive({
    req(input$sel_grupo_type)
    if(input$sel_grupo_type == "biologico"){
      return(input$sel_grupo_bio)
    } else {
      return(input$sel_grupo_int)
    }
    input$sel_grupo_bio
  })

  output$sel_tematica_ <- renderUI({
    req(opts_tematicas)
    default_select <- "todas"
    if (!is.null(url_par()$tematica)) default_select <- tolower(url_par()$tematica)
    radioButtons("sel_tematica", "Temática", opts_tematicas, selected = default_select)
  })




  ### INPUTS ##########

  inputs <- reactive({
    # req(input$sel_grupo_type)
    # req(input$sel_tipo)
    message("sel_tipo: ", input$sel_tipo)
    subregiones <- input$sugregiones %||% FALSE
    with_parent <- input$with_parent %||% FALSE
    grupo <- NULL
    grupo <- sel_grupo()
    if(!is.null(grupo)){
      if(grupo == "todos") grupo <- NULL
    }

    tematica <- input$sel_tematica
    if(!is.null(tematica)){
      if (tematica == "todas") tematica <- NULL
    }

    l <- list(
      region = input$sel_region,
      grupo = grupo,
      tipo = input$sel_tipo,
      #cobertura = input$sel_cobertura,
      tematica = tematica,
      subregiones = subregiones,
      with_parent = with_parent
    )
    l
  })

  is_amenazadas_or_cites_or_exoticas <- reactive({
    req(inputs())
    tematica <- inputs()$tematica
    if(is.null(tematica)) return(FALSE)
    (grepl("cites", tematica) ||
        grepl("amenazadas", tematica) #||
      #grepl("exoticas_total", input$sel_tematica)
    )
  })

  is_exotica <- reactive({
    req(inputs())
    tematica <- inputs()$tematica
    if(is.null(tematica)) return(FALSE)
    tematica %in% c("exoticas_total", "exoticas", "invasoras", "riesgo_invasion")
  })



  ### Available charts

  available_charts <- reactive({
    req(inputs())
    charts <- c( "Mapa" = "map", "Torta"= "pie", "Dona" = "donut",
                 "Treemap" = "treemap","Barras" = "bar", "Tabla"="table")
    map_table <- c("Mapa" = "map", "Tabla" = "table")
    map_table_bar <- c("Mapa" = "map", "Tabla" = "table", "Barras" = "bar")
    if(!is_amenazadas_or_cites_or_exoticas()){
      if(inputs()$tipo == "registros"){
        return(map_table)
      }
      if(inputs()$tipo == "especies"){
        return(map_table_bar)
      }
    }
    charts
  })

  output$viz_type <- renderUI({
    # images <- available_charts()
    # images <- c("Mapa" = "map", "Tabla" = "table")
    images <- c( "Mapa" = "map", "Torta"= "pie", "Dona" = "donut",
                 "Treemap" = "treemap","Barras" = "bar", "Tabla"="table")
    # av_charts <- c( "Mapa" = "map", "Tabla"="table")
    av_charts <- available_charts()
    active <- av_charts[1]

    buttonImageInput('chart_type',
                     # label = NULL,
                     images = images,
                     highlightColor = "#09A274",
                     button_width = 40,
                     path = 'www/viz_icons',
                     active = active,
                     disabled = images[!images %in% av_charts])
  })

  current_chart <- reactive({
    # req(input$chart_type)
    #input$chart_type %||% "map"
    input$chart_type
  })


  observe({
    r$show_subcategoria <- is_amenazadas_or_cites_or_exoticas() && current_chart() == "map"
    show_especies_total_estimadas <- current_chart() == "map" &&
      inputs()$tipo == "especies" && !is_amenazadas_or_cites_or_exoticas() &&
      is.null(inputs()$tematica)
    # if(!is.null(inputs()$tematica)){
    #   show_especies_total_estimadas <- show_especies_total_estimadas &&
    #     inputs()$tematica != "migratorias"
    # }
    message("show especies total estimada: ", show_especies_total_estimadas)
    # r$show_especies_total_estimadas <- ifelse(is.null(show_especies_total_estimadas), FALSE, TRUE)
    r$show_especies_total_estimadas <-show_especies_total_estimadas
  })



  #### DEBUG ######

  output$debug1 <- renderPrint({
    # str(input$sel_tematica)
    # str(input$sel_grupo_type)
    str("GRUPO")
    str(sel_grupo())
    # str(input$sel_grupo_bio)
    # str(input$sel_grupo_int)
    # str(input$chart_type)
    str("INDICADOR")
    str(r$indicador)

    str("IS AMENAZADAS CITES O EXÓTICAS")
    str(is_amenazadas_or_cites_or_exoticas())
    str("SHOW SUBCATEGORIA")
    str(r$show_subcategoria)
    str("SHOW ESPECIES TOTALES ESTIMADAS")
    str(r$show_especies_total_estimadas)
    str("CURRENT_CHART")
    #str(available_charts())
    str(current_chart())
    str("INPUTS")
    str(inputs())
    str("DATA_PARAMS")
    str(data_params())
  })




  ### DATA CONTROLS #####

  output$data_controls <- renderUI({
    out <- NULL
    req(inputs())
    # no req is_amenazadas_or_cites_or_exoticas() porque retorna TRUE or FALSE
    # req(current_chart())
    # req(input$chart_type)

    chart_type <- current_chart()

    if(r$show_subcategoria){
      if(grepl("amenazadas", input$sel_tematica)){
        out <- selectInput("amenazadas_categoria", "Categoría Amenaza",
                           c("Total amenazadas" = "_total", "EN" = "_en", "CR" = "_cr", "VU" = "_vu"))
      }else if(grepl("cites", input$sel_tematica)){
        out <- selectInput("cites_categoria", "Categoría CITES",
                           c("Total cites" = "_total", "I" = "_i", "I/II" = "_i_ii", "II" = "_ii", "III" = "_iii"))
      }else if(grepl("exoticas_total", inputs()$tematica)){
        out <- selectInput("exoticas_categoria", "Categoría CITES",
                           c("Total" = "_total"
                             # "Exóticas" = "exoticas",
                             # "Invasoras" = "invasoras",
                             # "Exóticas Riesgo Invación" = "riesgo_invasion"
                           ))
      }
    }
    if(r$show_especies_total_estimadas){
      message("show esp 2", r$show_especies_total_estimadas)
      out <- tagList(out, selectInput("especies_total_estimadas", "Total o Estimadas",
                                      c("Total" = "region_total",
                                        "Estimadas" = "region_estimadas"
                                      )))
    }
    out
  })





  ## DATA PARAMS ######

  data_params <- reactive({
    req(inputs())
    req(current_chart())
    input$chart_type
    inp <- inputs()
    tematica <- inp$tematica
    # if(is.null(inp$region)) return()


    # Update indicador
    r$indicador <- NULL
    r$amenazadas_categoria <- NULL
    r$cites_categoria <- NULL
    r$especies_total_estimadas <- NULL

    message("HERE")
    # Actualizar indicador solo para los mapas
    # TODO show the table with
    if(current_chart() == "map" ){
      message("  current map")
      # caso amanazadas cites o exoticas
      if(is_amenazadas_or_cites_or_exoticas()){
        message("    is ace")
        if(grepl("amenazadas", tematica)){
          message("      is_amenazada")
          r$indicador <- paste0(inputs()$tipo, "_", tematica, input$amenazadas_categoria)
          message("indicador", r$indicador)
          r$amenazadas_categoria <- input$amenazadas_categoria
        }
        if(grepl("cites", tematica)){
          r$indicador <- paste0(inputs()$tipo, "_", tematica, input$cites_categoria)
          r$cites_categoria <- input$cites_categoria
        }
        # if(grepl("exoticas_total", input$sel_tematica)){
        #   indicador <- paste0(input$sel_tipo, "_", tematica, input$exoticas_categoria)
        # }
      }else{
        # Case for non amenazadas, cites, exóticas
        if(inputs()$tipo == "especies" && is.null(inputs()$tematica)){
          r$indicador <- paste0(input$sel_tipo, "_", input$especies_total_estimadas)
        }

      }
      # Caso exóticas
      # Para ver los casos de exóticas
      if(is_exotica()){
        if(tematica %in% c("invasoras", "riesgo_invasion")) tematica <- "exoticas"
        indicador <-  paste0(input$sel_tipo, "_", inputs()$tematica)
        message("is exotica tematica: ", tematica)
        if(inputs()$tematica == "riesgo_invasion"){
          message("is_riesgo_invasion")
          indicador <-  paste0(input$sel_tipo, "_exoticas_", inputs()$tematica)
        }
        r$indicador <- indicador
        r$exotica_categoria <- inputs()$tematica
        message("is exotica: r$indicador: ", r$indicador)
      }
    }

    if(is_amenazadas_or_cites_or_exoticas()){
      r$especies_total_estimadas <- NULL
    }

    ## TODO actualizar este indicador también para tomar en cuenta lo que
    # viene de amenazadas, cites y exóticas

    subregiones <- FALSE
    if(current_chart() == "map"){
      subregiones <- TRUE
    }


    list(region = inp$region,
         grupo = inp$grupo,
         tipo = inp$tipo,
         cobertura = inp$cobertura,
         tematica = tematica,
         indicador = r$indicador,
         subregiones = subregiones,
         with_parent = inp$with_parent,
         con = con)
  })

  #### BREADCRUMBS

  output$breadcrumb <- renderText({
    req(data_params())
    text <- dstools::collapse(
      data_params()$region, data_params()$tipo,
      data_params()$grupo, data_params()$tematica,
      # r$exotica_categoria,
      r$amenazadas_categoria,
      r$cites_categoria,
      r$especies_total_estimadas,
      collapse = " | ")
    text <- gsub("_", " ", text)
    text <- gsub("-", " ", text)
    text <- toupper(text)
    text <- gsub("INVASION", "INVASIÓN", text)
    text <- gsub("ENDEMICA", "ENDÉMICA", text)
    text <- gsub("EXOTICA", "EXÓTICA", text)
    ##
    text <- gsub("ARACNIDOS", "ARÁCNIDOS", text)
    text <- gsub("CRUSTACEOS", "CRUSTÁCEOS", text)
    text <- gsub("DIPTEROS", "DÍPTEROS", text)
    text <- gsub("MAMIFEROS", "MAMÍFEROS", text)
    text <- gsub("DULCEACUICOLAS", "DULCEACUÍCOLAS", text)
    text <- gsub("HEPATICAS", "HEPÁTICAS", text)
    ##
    text <- gsub("LIQUENES", "LÍQUENES", text)
    text <- gsub("EPIFITAS", "EPÍFITAS", text)
    text <- gsub("ORQUIDEAS", "ORQUÍDEAS", text)
    text <- gsub("FANEROGAMAS", "FANERÓGAMAS", text)
    text <- gsub("DECAPODOS", "DECÁPODOS", text)

    text
  })



  ### DATA #########

  data <- reactive({
    if(is.null(data_params())) return()
    params <- data_params()
    message("Tematica:", params$tematica)
    message("Indicador:", params$indicador)
    d <- do.call("sibdata", params)

    if(current_chart() %in% c("pie", "donut", "treemap", "bar", "table")){
      d <- d |> sib_merge_ind_label(con = con)
    }

    # if(is_amenazadas_or_cites_or_exoticas() && chart_type == "map"){
    #   d <- data()
    # }
    # # If not is amenazadas_cites_exoticas
    # input$especies_total_estimadas
    d
  })




  ### DEBUG 2 #####
  output$debug2 <- renderPrint({
    str(is_amenazadas_or_cites_or_exoticas())
    str(current_chart())
    str(data())
    str(vizOps())
    # str(input$sel_tematica)
    #str(l_viz())
  })




  ### VIZ ###########




  vizOps <- reactive({
    req(data_params())
    req(current_chart())
    req(data())
    # req(available_charts())
    # req(actual_but$active)
    # req(r$active)
    dd <- data()
    params <- data_params()

    palette <- NULL
    color_by <- NULL

    # if(!is.null(r$inputs$tematica)){
    if(!is.null(params)){
      # if(grepl("amenazadas", r$inputs$tematica)){
      if(!is.null(params$tematica)){
        if(grepl("amenazadas", params$tematica)){
          palette <- c("#FF0000", "#FFA500", "#FFFF00")
          palette <- c("#d9453d", "#d8783d", "#d7a900")
          color_by <- 1
        }
        if(grepl("cites", params$tematica)){
          palette <- c("#00AFFF", "#000000", "#FFD150", "#4DD3AC")
          color_by <- 1
        }
      }
    }
    opts <- list(
      data = dd,
      color_palette_categorical = palette,
      color_by = color_by,
      con = con
    )

    if(current_chart() %in% c("pie", "donut")){
      opts <- c(opts, list(legend_align="right",
                           legend_vertical_align = "middle",
                           axis_text_wrap = 100))
    }

    opts <- dstools::removeNulls(opts)

    if(current_chart() == "map") {
      opts$region <- params$region
      opts$indicador <- params$indicador
      opts$conmap <- conmap
    }else{
      opts$con <- NULL
    }
    opts
  })




  l_viz <- reactive({
    req(vizOps())
    req(current_chart())
    # req(available_charts())
    # if(is.null(actual_but$active)) return()
    if(is.null(current_chart())) return()
    opts <- vizOps()
    # is_amenazadas_or_cites_or_exoticas()
    if ( current_chart() == "table") return()
    viz <- paste0("hgmagic::hg_", current_chart(), "_CatNum")
    if ( current_chart() == "map") viz <- "choropleth_map"
    do.call(eval(parse(text=viz)), opts)
    # NULL
  })


  output$hgch_viz <- renderHighchart({
    if(is.null(current_chart())) return()
    req(l_viz())
    if (current_chart() %in% c("table", "map")) return()
    l_viz()
  })

  output$lflt_viz <- renderLeaflet({
    # req(available_charts())
    if(is.null(current_chart())) return()
    # if(is.null(actual_but$active)) return()
    req(l_viz())
    if (current_chart() != "map") return()
    l_viz()
  })

  output$dt_sum <- renderDataTable({
    # req(data())
    # data()
    d <- data()
    nms <- names(d)
    nms <- sib_merge_ind_label(nms, con = con)
    nms[nms == "count"] <- "Número"
    nms[nms == "indicador"] <- "Indicador"
    names(d) <- nms
    DT::datatable(d,
                  rownames = F,
                  selection = 'none',
                  escape = FALSE,
                  #extensions = 'Buttons',
                  options = list(
                    dom = 'Bftsp',
                    #buttons = c('copy', 'csv'),
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    scrollX = T,
                    fixedColumns = TRUE,
                    fixedHeader = TRUE,
                    searching = FALSE,
                    info = FALSE,
                    #scrollY = "700px",
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#4ad3ac', 'color': '#ffffff'});",
                      "}")
                  ))


  })

  output$viz <- renderUI({
    # req(actual_but$active)
    # if(is.null(actual_but$active)) return()
    if(is.null(current_chart())) return()
    # if (actual_but$active == "table") {
    if (current_chart() == "table") {
      dataTableOutput("dt_sum")
      # } else if (actual_but$active == "map") {
    } else if (current_chart() == "map") {
      leafletOutput("lflt_viz", height = 450)
    } else {
      highchartOutput("hgch_viz", height = 450)
    }
  })

  ### VIZ DOWNLOAD #####

  output$descargas <- renderUI({
    req(current_chart())
    out <- NULL
    # if (current_chart() != "table") {
    #   # downloadImageUI("download_viz", dropdownLabel = "Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown")
    # } else {
    #   out <- downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown")
    # }
    # Dejar la descarga de datos siempre
    out <- downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown")
    out
  })

  downloadTableServer("dropdown_table", element = reactive(data()), formats = c("csv", "xlsx", "json"))
  # downloadImageServer("download_viz", element = reactive(l_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")


  output$data_viz <- renderDataTable({
    data()
  })



  ###### ESPECIES LIST #################

  data_especies <- reactive({
    req(input$sel_grupo_type)
    req(input$sel_tematica)
    grupo <-  input$sel_grupo_bio
    if (input$sel_grupo_type == "interes") grupo <- input$sel_grupo_int
    req(grupo)
    if (grupo == "todos") grupo <- NULL
    tematica <- gsub("_", "-", input$sel_tematica) ## OJO quitar cuando se estandarice _ y - en amenazadas_nacional
    if (tematica == "todas") tematica <- NULL
    l_s <- list_species(region = input$sel_region,
                        grupo = grupo,
                        tematica = tematica,
                        #with_labels = TRUE
                        con = con) |>
      collect() |>
      mutate(
      ) |>
      select(-species, -flagTAXO, -vernacular_name_es) |>
      select(-any_of(c("slug_especie", "slug_tematica"))) |>
      rename(
        "Especie" = "label",
        "Registros" = "registros",
        "Reino" = "kingdom",
        "GBIF" = "url_gbif",
        "CBC" = "url_cbc",
        "Filo" = "phylum",
        "Clase" = "class",
        "Orden" = "order",
        "Familia" = "family",
        "Género" = "genus"
      )
    l_s
  })

  output$list_species <- renderDataTable({

    req(data_especies())
    l_s <- data_especies()
    l_s$GBIF <- ifelse(is.na(l_s$GBIF), "", paste0("<a href='",l_s$GBIF,"'  target='_blank'>","GBIF","</a>"))
    l_s$CBC <- ifelse(is.na(l_s$CBC), "", paste0("<a href='",l_s$CBC,"'  target='_blank'>","CBC","</a>"))
    # l_s$CBC[l_s$CBC == "<a href='NA'  target='_blank'>NA</a>"] <- ""
    # l_s$GBIF[l_s$GBIF == "<a href='NA'  target='_blank'>NA</a>"] <- ""
    DT::datatable(l_s,
                  rownames = F,
                  selection = 'none',
                  escape = FALSE,
                  #extensions = 'Buttons',
                  options = list(
                    dom = 'Bftsp',
                    #buttons = c('copy', 'csv'),
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    scrollX = T,
                    fixedColumns = TRUE,
                    fixedHeader = TRUE,
                    searching = FALSE,
                    info = FALSE,
                    #scrollY = "700px",
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#4ad3ac', 'color': '#ffffff'});",
                      "}")
                  ))

  })
  downloadTableServer("species_table", element = reactive(data_especies()), formats = c("csv", "xlsx", "json"))


  # Ensure the connection is closed when the session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
    gt_discon(conmap)
  })


}

shinyApp(ui, server)
