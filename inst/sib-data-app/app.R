
library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)
library(hgchmagic)

ds <- read_rds("ds.rds")

opts_grupo_biologico <- c("todos", ds$grupo_biologico$slug)
opts_grupo_interes <-  c("todos", ds$grupo_interes_conservacion$slug)
opts_region <- ds$region$slug
opts_region <- c("colombia", "narino", "boyaca", "santander", "tolima",
                 "resguardo-indigena-pialapi-pueblo-viejo",
                 "reserva-natural-la-planada")



#     shinyinvoer::buttonImageInput(ns('viz_selection'),
#                                   " ",#div(class="title-data-select", "Selecciona tipo de visualización"),
#                                   images = possible_viz,
#                                   path = app_sys("app/www/viz_icons/"),#app_sys(paste0("app/www/viz_icons/", "reconocimientoFacialApp")),
#                                   active = actual_but$active,
#                                   #tooltips = viz_tool(),
#                                   imageStyle = list(borderColor = "#ffffff",
#                                                     borderSize = "1px",
#                                                     padding = "7px",
#                                                     shadow = TRUE)
#     )


ui <- panelsPage(
  panel(title = "Opciones", width = 250,
        body = div(
          # verbatimTextOutput("debug"),
          selectizeInput("sel_region","Seleccione Región",opts_region,
                         selected = "colombia"),
          hr(),
          radioButtons("sel_grupo_type", "Tipo de grupo",
                       c( "Biológico" = "biologico", "Interés de Conservación" = "interes")),
          conditionalPanel("input.sel_grupo_type == 'biologico'",
                           selectizeInput("sel_grupo_biologico","Seleccione grupo",opts_grupo_biologico)
          ),
          conditionalPanel("input.sel_grupo_type == 'interes'",
                           selectizeInput("sel_grupo_interes","Seleccione grupo",opts_grupo_interes)
          ),
          hr(),
          radioButtons("registro_especie", "Tipo registo", c("Todos" = "todos", "Observaciones" = "registro","Especies"="especie")),
          radioButtons("modo", "Modo", c("Todos" = "todos","Continental" = "continental","Marino" = "marinas")),
          radioButtons("tematica", "Temática", c("todas","amenazadas", "cites", "endemicas", "migratorias", "exoticas", "invasoras")),
          br()
        ),
        footer = ""),
  panel(title = "Información",
        body = div(
          uiOutput("controls"),
          uiOutput("chart_controls"),
          uiOutput("viz_type"),
          uiOutput("viz"),
          br()
        ),
        footer = "")
)

server <-  function(input, output, session) {


  output$debug <- renderText({
    #capture.output(str(data()))
    #glimpse(data())
    #input$sel_grupo
    available_chart_vars()
  })


  sel_grupo <- reactive({
    if(input$sel_grupo_type == "biologico"){
      return(input$sel_grupo_biologico)
    } else {
      return(input$sel_grupo_interes)
    }
  })

  d_gr <- reactive({
    if(input$sel_grupo_type == "biologico"){
      d <- ds$region_grupo_biologico
      if(sel_grupo() != "todos")
        d <- d |> filter(slug_grupo_biologico == sel_grupo())
    } else {
      d <- ds$region_grupo_interes_conservacion
      if(sel_grupo() != "todos")
        d <- d |> filter(slug_grupo_interes_conservacion == sel_grupo())
    }
    d
  })

  d_gr_reg <- reactive({
    d <- d_gr()
    ind_reg <- d |>
      filter(slug_region == input$sel_region)
    ind_reg
  })

  d_gr_subreg <- reactive({
    d <- d_gr()
    subregs <- ds$region |>
      filter(parent == input$sel_region) |>
      pull(slug)
    ind_subregs <- d |>
      filter(slug_region %in% subregs)
    ind_subregs
  })


  vars_meta <- reactive({
    inds <- ds$ind_meta
    if(input$tematica != "todas"){
      inds <- inds |>
        filter(grepl(input$tematica,tematica))
    }
    if(input$modo != "todos"){
      inds <- inds |>
        filter(grepl(input$modo,modo))
    }
    if(input$registro_especie != "todos"){
      inds <- inds |>
        filter(grepl(input$registro_especie,tipo))
    }
    inds |> pull(indicador)
  })


  data_selected <- reactive({
    #req(d_gr_reg())
    d <- d_gr_reg()
    if(input$region_type == "subregion"){
      d <- d_gr_subreg()
    }

    vars <- c("slug", vars_meta())

    d <- d |>
      select(contains(vars))

    d2 <- d |>
      pivot_longer(-starts_with("slug"),
                   names_to = c("indicador"),
                   values_to = "count")

    inds <- ds$ind_meta |>
      filter(indicador %in% names(d))
    d3 <- left_join(d2, inds)
    d4 <- d3 |>
      select(-indicador) |>
      select_if(~length(unique(.))!= 1)
    d5 <- d4 |>
      relocate(count, .after = last_col())
    return(d5)
  })

  available_chart_vars <- reactive({
    #req(data())
    d <- data_selected()
    d <- d |> select(-count)
    names(d)
  })

  data <- reactive({
    dd <- data_selected()
    dd <- dd |>
      select(c(any_of(input$chart_vars),"count"))
    dd
  })

  available_charts <- reactive({
    #dd <- data()
    c("table","pie", "donut", "treemap", "bar")
  })


  output$controls <- renderUI({
    out <- list()
    out <- list(
      radioButtons("region_type", "Tipo",
                   c( "Total región"="region", "Subregiones"="subregion")),
      br()
    )
    out
  })

  output$chart_controls <- renderUI({
    #req(available_chart_vars())
    selectizeInput("chart_vars","Seleccione variables a visualizar",
                   available_chart_vars(), multiple = FALSE,
                   selected = available_chart_vars()[2],
                   options = list(plugins = list('drag_drop')), width = 200)
  })

  output$viz_type <- renderUI({
    selectInput("sel_chart_type","Seleccione tipo de visualización",
                available_charts())
  })



  output$viz <- renderUI({

    dd <- data()

    sel_chart_type <- input$sel_chart_type

    opts <- list(
      dataLabels_show = TRUE,
      color_by = names(dd)[1]
    )
    out <- list(
      pie = renderHighchart(hgch_pie_CatNum(dd, opts = opts)),
      donut = renderHighchart(hgch_donut_CatNum(dd, opts = opts)),
      bar = renderHighchart(hgch_bar_CatNum(dd, opts = opts)),
      treemap = renderHighchart(hgch_treemap_CatNum(dd, opts = opts)),
      table = renderDataTable(dd)
    )
    out[[sel_chart_type]]

  })




}

shinyApp(ui, server)
