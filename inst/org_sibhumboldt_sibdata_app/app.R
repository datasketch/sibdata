
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

#dbdir <- sys_file_sibdata("db/sibdata.sqlite")
#dbdir <- sys_file_sibdata("db/sibdata.duckdb")
#dbdir <- "db/sibdata.duckdb"
#dbdir <- "inst/db/sibdata.duckdb"
#con <- duckdb_con(con = dbdir)
#duckdbits::duckdb_disconnect()
dbdir <- "db/sibdata.sqlite"
con <- DBI::dbConnect(RSQLite::SQLite(), dbdir,
                      read_only = TRUE)
#
# av_grupos_bio <- sib_available_grupos(tipo = "biologico", con = con)
# opts_grupo_biologico <- c("Todos" = "todos", av_grupos_bio)

av_grupos_int <- sib_available_grupos(tipo = "interes", con = con)
opts_grupo_interes <-  c("Todos" = "todos", av_grupos_int)

#
gru <- sibdata_grupo(con) |> collect() |> filter(tipo == "biologico")
gru_tree <- data.tree::FromDataFrameNetwork(gru)
gru_df <- data.tree::ToDataFrameNetwork(gru_tree,
                                        direction = "descend",
                                        "label", "level", "path")
paste_dash <- function(str, times = 1){
  paste(" ", paste0(rep("-",times-1), collapse = ""),str)
}
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
# opts_region <- c("colombia", "narino", "boyaca", "santander", "tolima",
#                  "resguardo-indigena-pialapi-pueblo-viejo",
#                  "reserva-natural-la-planada")


opts_tematicas <- c("Todas" = "todas", sib_available_tematicas())
opts_tematicas_ex <- c("cites_i", "cites_ii","cites_i_ii", "cites_iii",
                       "exoticas_total",
                       "exoticas", "invasoras", "riesgo_invasion"
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
          verbatimTextOutput("debug"),
          verbatimTextOutput("debug2"),
          uiOutput("sel_region_"),
          hr(),
          uiOutput("sel_grupo_"),
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
                             uiOutput("viz_type")),
                           div(class='second-container',
                               uiOutput("descargas"))
        ),
        body = div(
          radioButtons("sel_tipo", "Tipo", c("Observaciones" = "registros","Especies"="especies")),
          hr(),
          br(),
          textOutput("breadcrumb"),
          br(),
          uiOutput("viz_controls"),
          #uiOutput("chart_controls"),
          uiOutput("viz"),
          br()
        ),
        footer = ""),
  panel(title = "Especies",
        width = 400,
        can_collapse = FALSE,
        header_right = downloadTableUI("species_table", dropdownLabel = "Descargar especies", formats = c("csv", "xlsx", "json"), display = "dropdown", dropdownWidth = 200),
        body = dataTableOutput("list_species")
  )
)

## SERVER ############

server <-  function(input, output, session) {

  par <- list(region = NULL, tematica = NULL, grupo = NULL)
  url_par <- reactive({
    url_params(par, session)$inputs
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


  output$sel_grupo_ <- renderUI({
    radioButtons("sel_grupo_type", "Tipo de grupo",
                 c("Biológico" = "biologico", "Interés de Conservación" = "interes"))
  })

  sel_grupo <- reactive({
    if(input$sel_grupo_type == "biologico"){
      return(input$sel_grupo_biologico)
    } else {
      return(input$sel_grupo_interes)
    }
  })

  output$sel_grupo_opts <- renderUI({
    req(input$sel_grupo_type)
    default_select <- NULL
    if (!is.null(url_par()$grupo)) default_select <- tolower(url_par()$grupo)
    opts <- opts_grupo_interes
    id <- "sel_grupo_int"
    out <- selectInput(id ,
                       "Seleccione grupo de interés",
                       opts_grupo_biologico,
                       default_select)
    if (input$sel_grupo_type == "biologico") {
      id <- "sel_grupo_bio"
      out <- selectInput(id ,
                         "Seleccione grupo biológico",
                         opts_grupo_biologico,
                         default_select)
    }
    out
  })

  output$sel_tematica_ <- renderUI({
    req(opts_tematicas)
    default_select <- NULL
    if (!is.null(url_par()$tematica)) default_select <- tolower(url_par()$tematica)
    radioButtons("sel_tematica", "Temática", opts_tematicas, selected = default_select)
  })

  ### INPUTS ##########

  inputs <- reactive({
    input$sel_tematica
    ## Inputs
    # req(input$sel_grupo_type)
    # req(input$sel_grupo_bio)
    # req(input$sel_grupo_int)
    # req(input$sel_tematica)
    subregiones <- input$sugregiones %||% FALSE
    with_parent <- input$with_parent %||% FALSE
    grupo <- NULL
    if(!is.null(input$sel_grupo_bio) && !is.null(input$sel_grupo_int)){
      grupo <- ifelse(input$sel_grupo_type == "biologico",
                      input$sel_grupo_bio, input$sel_grupo_int)
      if(grupo == "todos") grupo <- NULL
    }
    tematica <- input$sel_tematica
    if(!is.null(tematica)){
      if (tematica == "todas") tematica <- NULL
    }
    indicador <- NULL
    # if(is.null(actual_but$active)) return()

    # if(r$active == "map"){
    # indicador <- paste(input$sel_tipo, tematica, input$tematica, sep = "_")
    # if(is.null(is_amenazadas_or_cites_or_exoticas())) return()
    if(is_amenazadas_or_cites_or_exoticas()){
      if(grepl("amenazadas", input$sel_tematica)){
        if(is.null(input$amenazadas_categoria)) return()
        indicador <- paste0(input$sel_tipo, "_", tematica, input$amenazadas_categoria)
      }
      if(grepl("cites", input$sel_tematica)){
        if(is.null(input$cites_categoria)) return()
        indicador <- paste0(input$sel_tipo, "_", tematica, input$cites_categoria)
      }
      # if(grepl("exoticas_total", input$sel_tematica)){
      #   if(is.null(input$exoticas_categoria)) return()
      #   indicador <- paste0(input$sel_tipo, "_", tematica, input$exoticas_categoria)
      # }
    }
    # }

    l <- list(
      region = input$sel_region,
      grupo = grupo,
      tipo = input$sel_tipo,
      #cobertura = input$sel_cobertura,
      tematica = tematica,
      indicador = indicador,
      subregiones = subregiones,
      with_parent = with_parent
    )
    # r$inputs <- l
    l
  })

  is_amenazadas_or_cites_or_exoticas <- reactive({
    req(input$sel_tematica)
    (grepl("cites", input$sel_tematica) ||
        grepl("amenazadas", input$sel_tematica) #||
      #grepl("exoticas_total", input$sel_tematica)
    )
  })




  ## DATA ######

  data <- reactive({
    req(inputs())
    # reset active chart
    if(!is.null(inputs()$indicador)){
      if(is.null(input$amenazadas_categoria) && is.null(input$cites_categoria)) return()
    }
    # inp <- r$inputs
    inp <- inputs()
    subR <- inp$subregiones
    d <- sibdata(inp$region,
                 grupo = inp$grupo,
                 tipo = inp$tipo,
                 cobertura = inp$cobertura,
                 tematica = inp$tematica,
                 # indicador = inp$indicador,
                 subregiones = subR,
                 with_parent = inp$with_parent,
                 con = con)
    d <- d |> sib_merge_ind_label(con = con)

    # actual_but$active <- "table"
    d
  })


  #### DEBUG ######

  output$debug <- renderPrint({
    str(input$sel_tematica)
    str(is_amenazadas_or_cites_or_exoticas())
    str(available_charts())
    str(input$sel_chart_type)
    str(current_chart())
    # str(actual_but$charts)
    # str(actual_but$prev_charts)
    # str(actual_but$active)
    # str(actual_but$prev_active)
    # str(actual_but$charts)
    str(inputs())
    # str(actual_but$active)
    # str(inputs$amenazadas_categoria)
    # str(data())
    # str(data_especies())
    # str(names(opts_grupo_biologico))
    # str(length(opts_grupo_biologico))
  })

  #### AVAILABLE CHARTS ###########

  available_charts <- reactive({
    # req(data())
    # input$sel_tematica
    # if(is.null(data())) return(c("Map" = "map", "Tabla" = "table"))
    # req(is_amenazadas_or_cites_or_exoticas())
    if(is.null(data())) return()
    dd <- data()
    if(nrow(dd) == 1){
      return(c("Map" = "map", "Tabla" = "table"))
    }
    charts <- c( "Mapa" = "map", "Torta"= "pie", "Dona" = "donut", "Treemap" = "treemap","Barras" = "bar", "Tabla"="table")

    if(!is_amenazadas_or_cites_or_exoticas()){
      return(c("Map" = "map", "Tabla" = "table"))
    }
    #c("Torta"= "pie", "Dona" = "donut", "Treemap" = "treemap","Barras" = "bar", "Tabla"="table")
    charts
  })

  actual_but <- reactiveValues(active = NULL, prev_active = NULL,
                               charts = NULL, prev_charts = NULL)

  current_chart <- reactive({
    if(is.null(available_charts())) return()
    if(is.null(input$sel_chart_type)) return()
    if(input$sel_chart_type %in% available_charts()){
      return(input$sel_chart_type)
    }else{
      return(available_charts()[1])
    }
  })

  observe({
    req(available_charts())
    req(current_chart())
    # active <- unname(available_charts())[1]
    if(current_chart() %in% available_charts()){
      new_active <- current_chart()
    }else{
      new_active <- available_charts()[1]
    }
    updateButtonImageInput(session, "sel_chart_type", active = new_active)
    # actual_but$active <- input$sel_chart_type
  })


  observe({
    # actual_but$prev_active <- actual_but$active
    # actual_but$active <- current_chart()
    # actual_but$prev_charts <- actual_but$charts

    # viz_rec <- available_charts()
    # if (is.null(input$sel_chart_type)) return()
    # vizDef <- input$sel_chart_type
    #
    # if (vizDef %in% viz_rec) {
    #   actual_but$active <- vizDef
    # } else {
    #   actual_but$active <- viz_rec[1]
    # }


    # req(available_charts())
    # if (is.null(input$sel_chart_type)) return()
    # actual_but$active <- input$sel_chart_type
    # # if (is.null(available_charts())) return()
    # actual_but$prev_charts <- actual_but$charts
    # actual_but$charts <- available_charts()

    # if(!actual_but$active %in% actual_but$charts){
    #actual_but$active <- actual_but$charts[1]
    # actual_but$active <- "table"
    # }

    # viz_rec <- available_charts() |> as.vector()
    # vizDef <- input$sel_chart_type
    # if (vizDef %in% viz_rec) {
    #   actual_but$active <- vizDef
    # } else {
    #   actual_but$active <- viz_rec[1]
    # }
  })
  # observe({
  #   # input$select_tematica
  #   ## Active MAP
  #   # r$active <- input$sel_chart_type
  #   # r$inputs <- inputs()
  #   # req(available_charts())
  #   # if (is.null(available_charts())) return()
  #   viz_rec <- available_charts() |> as.vector()
  #   if (is.null(input$sel_chart_type)) return()
  #   vizDef <- input$sel_chart_type
  #
  #   if (vizDef %in% viz_rec) {
  #     r$active <- vizDef
  #   } else {
  #     r$active <- viz_rec[1]
  #   }
  # })



  ### DEBUG 2 #####
  output$debug2 <- renderPrint({
    str(vizOps())
    str(input$sel_tematica)
    str(is_amenazadas_or_cites_or_exoticas())
    #str(l_viz())
  })


  ### VIZ ###########

  output$breadcrumb <- renderText({
    req(inputs())
    # text <- dstools::collapse(
    #   names(inputs()$region), names(inputs()$tipo),
    #   names(inputs()$grupo), names(inputs()$tematica),
    #   collapse = " | ")
    text <- dstools::collapse(
      inputs()$region, inputs()$tipo, inputs()$grupo, inputs()$tematica,
      collapse = " | ")
    text <- gsub("_", " ", text)
    toupper(text)
  })


  output$viz_type <- renderUI({
    req(available_charts())
    # req(input$sel_tematica)

    #active <- actual_but$active
    active <- NULL

    images <- available_charts()
    # active <- available_charts()[1]
    buttonImageInput('sel_chart_type',
                     # label = NULL,
                     images = images,
                     highlightColor = "#09A274",
                     path = 'viz_icons/',
                     active = active)
  })

  output$viz_controls <- renderUI({
    out <- NULL
    # req(inputs())
    req(input$sel_tematica)
    if(is_amenazadas_or_cites_or_exoticas()){
      if(grepl("amenazadas", input$sel_tematica)){
        out <- selectInput("amenazadas_categoria", "Categoría Amenaza",
                           c("EN" = "_en", "CR" = "_cr", "VU" = "_vu"))
      }else if(grepl("cites", input$sel_tematica)){
        out <- selectInput("cites_categoria", "Categoría CITES",
                           c("I" = "_i", "I/II" = "_i_ii", "II" = "_ii", "III" = "_iii"))
      }else if(grepl("exoticas_total", input$sel_tematica)){
        out <- selectInput("exoticas_categoria", "Categoría CITES",
                           c("Total" = "_total"
                             # "Exóticas" = "exoticas",
                             # "Invasoras" = "invasoras",
                             # "Exóticas Riesgo Invación" = "riesgo_invasion"
                           ))
      }
      else{
        out <- NULL
      }
    }
    out
  })

  vizOps <- reactive({
    req(inputs())
    req(data())
    req(current_chart())
    # req(available_charts())
    # req(actual_but$active)
    input$sel_chart_type
    # req(r$active)
    dd <- data()

    palette <- NULL
    # if(!is.null(r$inputs$tematica)){
    if(!is.null(inputs())){
      # if(grepl("amenazadas", r$inputs$tematica)){
      if(!is.null(inputs()$tematica)){
        if(grepl("amenazadas", inputs()$tematica)){
          palette <- c("#FF0000", "#FFA500", "#FFFF00")
          palette <- c("#d9453d", "#d8783d", "#d7a900")
        }
        if(grepl("cites", inputs()$tematica)){
          palette <- c("#00AFFF", "#000000", "#FFD150", "#4DD3AC")
        }
      }
    }


    opts <- list(
      data = dd,
      color_palette_categorical = palette
    )

    # if(actual_but$active == "map") {
    if(current_chart() == "map") {
      # opts <- r$inputs
      opts <- inputs()
      opts <- c(opts, con = con)
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
    req(available_charts())
    # if(is.null(actual_but$active)) return()
    if(is.null(current_chart())) return()
    req(l_viz())
    if (current_chart() %in% c("table", "map")) return()
    l_viz()
  })

  output$lflt_viz <- renderLeaflet({
    req(available_charts())
    if(is.null(current_chart())) return()
    # if(is.null(actual_but$active)) return()
    req(l_viz())
    if (current_chart() != "map") return()
    l_viz()
  })

  output$dt_sum <- renderDataTable({
    req(data())
    data()
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
      leafletOutput("lflt_viz", height = 500)
    } else {
      highchartOutput("hgch_viz", height = 500)
    }
  })

  ### VIZ DOWNLOAD #####

  output$descargas <- renderUI({
    out <- NULL
      if (current_chart() != "table") {
        # downloadImageUI("download_viz", dropdownLabel = "Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown")
      } else {
        downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown")
      }
  })

  downloadTableServer("dropdown_table", element = reactive(data()), formats = c("csv", "xlsx", "json"))
  # downloadImageServer("download_viz", element = reactive(l_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")


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
    l_s$GBIF <- paste0("<a href='",l_s$GBIF,"'  target='_blank'>","GBIF","</a>")
    l_s$CBC <- paste0("<a href='",l_s$CBC,"'  target='_blank'>","CBC","</a>")
    l_s$CBC[l_s$CBC == "<a href='NA'  target='_blank'>NA</a>"] <- ""
    l_s$GBIF[l_s$GBIF == "<a href='NA'  target='_blank'>NA</a>"] <- ""
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





}

shinyApp(ui, server)
