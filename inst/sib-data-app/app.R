
library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)
library(hgchmagic)
library(lfltmagic)
library(sibdata)
library(shinyinvoer)
library(shinydisconnect)

opts_grupo_biologico <- c("Todos" = "todos", sib_available_grupos(tipo = "biologico"))
opts_grupo_interes <-  c("Todos" = "todos", sib_available_grupos(tipo = "interes"))

opts_region <- sib_available_regions(subtipo = "Departamento")
# opts_region <- c("colombia", "narino", "boyaca", "santander", "tolima",
#                  "resguardo-indigena-pialapi-pueblo-viejo",
#                  "reserva-natural-la-planada")

opts_tematicas <- sib_available_tematicas()


ui <- panelsPage(
  disconnectMessage(
    text = "Tu sesión ha finalizado, por favor haz click aquí para recargar vista",
    refresh = "RECARGAR",
    background = "#ffffff",
    colour = "#da1c95",
    size = 14,
    overlayColour = "#2a2e30",
    overlayOpacity = 0.85,
    refreshColour = "#ffffff",
    css = "padding: 4.8em 3.5em !important; box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important;"
  ),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),
  panel(title = "Opciones", width = 300,
        body = div(
          verbatimTextOutput("debug"),
          selectizeInput("sel_region","Seleccione Región",
                         rev(opts_region),
                         selected = "Tolima"
          ),
          hr(),
          radioButtons("sel_grupo_type", "Tipo de grupo",
                       c("Biológico" = "biologico", "Interés de Conservación" = "interes")),
          conditionalPanel("input.sel_grupo_type == 'biologico'",
                           selectizeInput("sel_grupo_bio","Seleccione grupo",
                                          opts_grupo_biologico)
          ),
          conditionalPanel("input.sel_grupo_type == 'interes'",
                           selectizeInput("sel_grupo_int","Seleccione grupo",
                                          opts_grupo_interes)
          ),
          hr(),
          radioButtons("sel_tipo", "Tipo", c("Observaciones" = "registros","Especies"="especies")),
          radioButtons("sel_cobertura", "Cobertura", c("Total" = "total","Continental" = "continentales","Marina" = "marinas")),
          radioButtons("sel_tematica", "Temática", opts_tematicas),
          br()
        ),
        footer = ""),
  panel(title = "Gráficos",
        can_collapse = FALSE,
        header_right = uiOutput("viz_type"),
        body = div(
          uiOutput("controls"),
          uiOutput("chart_controls"),

          uiOutput("viz"),
          br()
        ),
        footer = ""),
  panel(title = "Especies",
        width = 300,
        body = dataTableOutput("list_species")
  )
)

server <-  function(input, output, session) {


  output$debug <- renderText({
    #capture.output(str(data()))
    #glimpse(data())
    #input$sel_grupo
    #glimpse(inputs())
    #summary(list(a=1, b= "x"))
    what <- c(input$sel_grupo_type, inputs())
    what <- data()
    what <- inputs()
    what <- input$viz_selection
    paste0(capture.output(what),collapse = "\n")
  })

  inputs <- reactive({

    subregiones = input$sugrebiones %||% FALSE
    with_parent = input$with_parent %||% FALSE

    grupo <- ifelse(input$sel_grupo_type == "biologico",
                    input$sel_grupo_bio, input$sel_grupo_int)
    if(grupo == "todos") grupo <- NULL

    list(
      region = input$sel_region,
      grupo = grupo,
      tipo = input$sel_tipo,
      cobertura = input$sel_cobertura,
      tematica = input$sel_tematica,
      subregiones = subregiones,
      with_parent = with_parent
    )
  })


  output$list_species <- renderDataTable({

    grupo <- ifelse(input$sel_grupo_type == "biologico",
                    input$sel_grupo_bio, input$sel_grupo_int)
    if (grupo == "todos") grupo <- NULL
    print(grupo)
    l_s <- list_species(region = input$sel_region,
                        grupo = grupo,
                        tematica = input$sel_tematica) |>
      collect()
    #tx <- "No hay especies registradas para los filtros seleccionados"

    #if (nrow(l_s) != 0) {
      DT::datatable(l_s,
                    rownames = F,
                    selection = 'none',
                    escape = FALSE,
                    options = list(
                      dom = 'Bfrtip',
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
      # tx <- HTML(paste0(
      #   purrr::map(unique(l_s$family), function(f){
      #     df <- l_s |> dplyr::filter(family %in% f)
      #     HTML(paste0("<p><b>Familia: ", unique(f),"</b><br/>",
      #                 paste0(df$species, " (", df$registros, ")", collapse = "<br/>"), "</p>", collapse = "<br/>"))
      #   }), collapse = "<br/>"))
    #}

    #tx
  })

  data <- function(){
    #req(inputs)
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

    print(class(d))
    d
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
    c("Torta"= "pie","Tabla"="table", "Dona" = "donut", "Treemap" = "treemap","Barras" = "bar", "Mapa" = "map")
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



  output$viz <- renderUI({
    req(data())
    dd <- data()

    sel_chart_type <- actual_but$active

    opts <- list(
      dataLabels_show = TRUE,
      color_by = names(dd)[1],
      legend_show = FALSE,
      text_family = "Lato"
    )

    if (actual_but$active == "map") {
    region <- inputs()$region
    opts$color_by <- NULL
    opts$map_name <- paste0("col_depto_", region)
    }


    out <- list(
      pie = renderHighchart(hgch_pie_CatNum(dd, opts = opts)),
      donut = renderHighchart(hgch_donut_CatNum(dd, opts = opts)),
      bar = renderHighchart(hgch_bar_CatNum(dd, opts = opts)),
      treemap = renderHighchart(hgch_treemap_CatNum(dd, opts = opts)),
      table = renderDataTable(dd),
      map = renderLeaflet(lflt_choropleth_GnmNum(dd, opts = opts))
    )
    out[[sel_chart_type]]

  })




}

shinyApp(ui, server)
