
library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)
library(hgchmagic)
library(lfltmagic)
library(sibdata)
library(shinyinvoer)
library(dsmodules)

opts_grupo_biologico <- c("Todos" = "todos", sib_available_grupos(tipo = "biologico"))
opts_grupo_interes <-  c("Todos" = "todos", sib_available_grupos(tipo = "interes"))

opts_region <- c(sib_available_regions(subtipo = "País"), sib_available_regions(subtipo = "Departamento"))
# opts_region <- c("colombia", "narino", "boyaca", "santander", "tolima",
#                  "resguardo-indigena-pialapi-pueblo-viejo",
#                  "reserva-natural-la-planada")

opts_tematicas <- c("Todas" = "todas", sib_available_tematicas())


ui <- panelsPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),
  panel(title = "Opciones", width = 250,
        body = div(
          #verbatimTextOutput("debug"),
          uiOutput("sel_region_"),
          hr(),
          uiOutput("sel_grupo_"),
          uiOutput("sel_grupo_opts"),
          hr(),
          radioButtons("sel_tipo", "Tipo", c("Observaciones" = "registros","Especies"="especies")),
          #radioButtons("sel_cobertura", "Cobertura", c("Total" = "total","Continental" = "continentales","Marina" = "marinas")),
          uiOutput("sel_tematica_")
          ,
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
                               uiOutput("descargas"))),
        body = div(
          uiOutput("controls"),
          uiOutput("chart_controls"),

          uiOutput("viz"),
          br()
        ),
        footer = ""),
  panel(title = "Especies",
        width = 450,
        can_collapse = FALSE,
        header_right = downloadTableUI("species_table", dropdownLabel = "Descargar especies", formats = c("csv", "xlsx", "json"), display = "dropdown", dropdownWidth = 200),
        body = dataTableOutput("list_species")
  )
)

server <-  function(input, output, session) {


  par <- list(region = NULL, tematica = NULL, grupo = NULL)
  url_par <- reactive({
    url_params(par, session)$inputs
  })

  output$debug <- renderPrint({
    #capture.output(str(data()))
    #glimpse(data())
    #input$sel_grupo
    #glimpse(inputs())
    #summary(list(a=1, b= "x"))
    # what <- c(input$sel_grupo_type, inputs())
    # what <- data()
    # what <- inputs()
    # what <- input$viz_selection
    # paste0(capture.output(what),collapse = "\n")
    url_par()
  })


  output$sel_region_ <- renderUI({
    req(opts_region)
    default_select <- NULL
    if (!is.null(url_par()$region)) default_select <- tolower(url_par()$region)
    selectizeInput("sel_region","Seleccione Región",
                   rev(opts_region),
                   selected = default_select
    )
  })


  output$sel_grupo_ <- renderUI({

    radioButtons("sel_grupo_type", "Tipo de grupo",
                 c("Biológico" = "biologico", "Interés de Conservación" = "interes"))
  })


  output$sel_grupo_opts <- renderUI({
    req(input$sel_grupo_type)
    default_select <- NULL
    if (!is.null(url_par()$grupo)) default_select <- tolower(url_par()$grupo)

    opts <- opts_grupo_interes
    id <- "sel_grupo_int"
    if (input$sel_grupo_type == "biologico") {
      opts <- opts_grupo_biologico
      id <- "sel_grupo_bio"
    }

    selectizeInput(id ,
                   "Seleccione grupo",
                   opts,
                   default_select)

  })



  output$sel_tematica_ <- renderUI({
    req(opts_tematicas)
    default_select <- NULL
    if (!is.null(url_par()$tematica)) default_select <- tolower(url_par()$tematica)
    radioButtons("sel_tematica", "Temática", opts_tematicas, selected = default_select)
  })

  inputs <- reactive({

    req(input$sel_grupo_type)
    req(input$sel_tematica)
    subregiones = input$sugrebiones %||% FALSE
    with_parent = input$with_parent %||% FALSE

    grupo <- ifelse(input$sel_grupo_type == "biologico",
                    input$sel_grupo_bio, input$sel_grupo_int)
    if(grupo == "todos") grupo <- NULL
    tematica <- input$sel_tematica
    if (tematica == "todas") tematica <- NULL

    list(
      region = input$sel_region,
      grupo = grupo,
      tipo = input$sel_tipo,
      #cobertura = input$sel_cobertura,
      tematica = tematica,
      subregiones = subregiones,
      with_parent = with_parent
    )
  })


  data_especies <- reactive({
    req(input$sel_grupo_type)
    req(input$sel_tematica)
    grupo <-  input$sel_grupo_bio
    if (input$sel_grupo_type == "interes") grupo <- input$sel_grupo_int
    req(grupo)
    if (grupo == "todos") grupo <- NULL
    tematica <- input$sel_tematica
    if (tematica == "todas") tematica <- NULL
    l_s <- list_species(region = input$sel_region,
                        grupo = grupo,
                        tematica = tematica,
                        with_labels = TRUE) |>
      collect()
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

  data <- function(){
    tryCatch({
    inp <- inputs()
    subR <- inp$subregiones
    req(actual_but$active)
    if (actual_but$active == "map") subR <- TRUE

    d <- sibdata(inp$region,
                 grupo = inp$grupo,
                 tipo = inp$tipo,
                 cobertura = inp$cobertura,
                 tematica = inp$tematica,
                 subregiones = subR,
                 with_parent = inp$with_parent)
    if (actual_but$active != "map") {
      d <- d |> sib_merge_ind_label()
    } else {
      d <- d |> dplyr::select(label, count)
    }

    #print(class(d))
    d
    },
    error = function(cond) {
      return()
    })
  }


  sel_grupo <- reactive({
    if(input$sel_grupo_type == "biologico"){
      return(input$sel_grupo_biologico)
    } else {
      return(input$sel_grupo_interes)
    }
  })

  available_charts <- reactive({
    #dd <- data()
    c( "Mapa" = "map", "Torta"= "pie", "Dona" = "donut", "Treemap" = "treemap","Barras" = "bar", "Tabla"="table")
  })


  hover_viz <- reactive({
    req(available_charts())
    names(available_charts())
  })


  actual_but <- reactiveValues(active = NULL)

  observe({
    if (is.null(available_charts())) return()
    viz_rec <- available_charts() |> as.vector()
    if (is.null(input$sel_chart_type)) return()
    vizDef <- input$sel_chart_type

    if (vizDef %in% viz_rec) {
      actual_but$active <- vizDef
    } else {
      actual_but$active <- viz_rec[1]
    }
  })


  output$viz_type <- renderUI({
    req(available_charts())
    suppressWarnings(
      buttonImageInput('sel_chart_type',
                       " ",
                       images = available_charts() |> as.vector(),
                       tooltips = hover_viz(),
                       path = 'viz_icons/',
                       active = actual_but$active)
    )
  })



  output$controls <- renderUI({
    # out <- list()
    # out <- list(
    #   radioButtons("region_type", "Tipo",
    #                c( "Total región"="region", "Subregiones"="subregion")),
    #   br()
    # )
    # out
  })

  output$chart_controls <- renderUI({
    #req(available_chart_vars())
    # selectizeInput("chart_vars","Seleccione variables a visualizar",
    #                available_chart_vars(), multiple = FALSE,
    #                selected = available_chart_vars()[2],
    #                options = list(plugins = list('drag_drop')), width = 200)
  })

  # output$viz_type <- renderUI({
  #   selectInput("sel_chart_type","Seleccione tipo de visualización",
  #               available_charts())
  # })


  vizOps <- reactive({
    req(data())
    req(actual_but$active)
    dd <- data()
    opts <- list(
      data = dd,
      dataLabels_show = TRUE,
      color_by = names(dd)[1],
      legend_show = FALSE,
      text_family = "Lato",
      axis_line_y_size = 1,
      axis_line_x_size = 1,
      axis_line_color = "#dbd9d9",
      border_weight = 0.2,
      grid_y_color = "#dbd9d9",
      grid_x_width = 0,
      palette_colors = c("#5151f2", "#4ad3ac", "#ffd150", "#00afff", "#ffe0bb", "#f26330", "#163875")
    )

    if (actual_but$active == "map") {
      region <- inputs()$region
      opts$color_by <- NULL
      opts$legend_show <- TRUE
      opts$palette_colors <- rev(c("#f26330", "#f77e38", "#fb9745", "#feae56", "#ffc570", "#ffdb93", "#ffeec9"))
      opts$map_name <- paste0("col_depto_", region)
      if (region == "colombia") opts$map_name <- "col_departments"
      opts$topo_fill_opacity <- 0.6
      opts$max_topo_fill_opacity <- 0.8
      opts$map_opacity <- 0.5
    }
    opts
  })


  l_viz <- reactive({
    req(vizOps())
    opts <- vizOps()
    sel_chart_type <- actual_but$active
    if (sel_chart_type == "table") return()
    viz <- paste0("hgchmagic::hgch_", sel_chart_type, "_CatNum")
    if (sel_chart_type == "map") viz <- "lfltmagic::lflt_choropleth_GnmNum"
    suppressWarnings(do.call(eval(parse(text=viz)), opts))
  })


  output$hgch_viz <- renderHighchart({
    req(l_viz())
    sel_chart_type <- actual_but$active
    if (sel_chart_type %in% c("table", "map")) return()
    l_viz()
  })

  output$lflt_viz <- renderLeaflet({
    req(l_viz())
    sel_chart_type <- actual_but$active
    if (!sel_chart_type %in% c( "map")) return()
    l_viz()
  })

  output$dt_sum <- renderDataTable({
    req(data())
    data()
  })

  output$viz <- renderUI({
    req(actual_but$active)
    if (actual_but$active == "table") {
      dataTableOutput("dt_sum")
    } else if (actual_but$active == "map") {
      leafletOutput("lflt_viz", height = 600)
    } else {
      highchartOutput("hgch_viz", height = 600)
    }
  })


  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      downloadImageUI("download_viz", dropdownLabel = "Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown")
    } else {
      downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown")
    }
  })

  downloadTableServer("dropdown_table", element = reactive(data_fin()), formats = c("csv", "xlsx", "json"))
  downloadImageServer("download_viz", element = reactive(l_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  downloadTableServer("species_table", element = reactive(data_especies()), formats = c("csv", "xlsx", "json"))



}

shinyApp(ui, server)
