
library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)
library(hgmagic)
# library(lfltmagic)
library(shinyinvoer)
library(dsmods)
library(geotable)
library(sibdata)
library(data.tree)

# dbdir <- sys_file_sibdata("db/sibdata.sqlite")
#dbdir <- sys_file_sibdata("db/sibdata.duckdb")
dbdir <- "db/sibdata.duckdb"
con <- gt_con(con = dbdir)
#duckdbits::duckdb_disconnect()
# dbdir <- "db/sibdata.sqlite"
# con <- DBI::dbConnect(RSQLite::SQLite(), dbdir,
#                       read_only = TRUE)

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
                       "exoticas", "invasoras", "riesgo_invasion")
opts_tematicas <- opts_tematicas[!opts_tematicas %in% opts_tematicas_ex]
opts_tematicas <- c(opts_tematicas, c("Exóticas Total" = "exoticas-total"))
# opts_tematicas <- gsub("_","-",opts_tematicas) # hay diferencia entre sibdata y list_species
# uno recibe _ y el otro -

ui <- panelsPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),
  panel(title = "Opciones", width = 280,
        body = div(
          # verbatimTextOutput("debug"),
          uiOutput("sel_region_"),
          hr(),
          uiOutput("sel_grupo_"),
          uiOutput("sel_grupo_opts"),
          hr(),
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
                               uiOutput("descargas"))
        ),
        body = div(
          radioButtons("sel_tipo", "Tipo", c("Observaciones" = "registros","Especies"="especies")),
          hr(),
          uiOutput("controls"),
          uiOutput("chart_controls"),
          uiOutput("viz"),
          br()
        ),
        footer = ""),
  panel(title = "Especies",
        width = 400,
        can_collapse = FALSE,
        # header_right = downloadTableButtonUI("species_table", dropdownLabel = "Descargar especies", formats = c("csv", "xlsx", "json"), display = "dropdown", dropdownWidth = 200),
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
    #url_par()
    #str(r$toList)
    str(inputs())
    # str(data())
    str(data_especies())
    str(names(opts_grupo_biologico))
    str(length(opts_grupo_biologico))
  })


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

  inputs <- reactive({

    req(input$sel_grupo_type)
    req(input$sel_tematica)
    req(input$sel_grupo_bio)
    # req(input$sel_grupo_int)

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

  data <- function(){
    req(inputs())
    inp <- inputs()
    subR <- inp$subregiones
    d <- sibdata(inp$region,
                 grupo = inp$grupo,
                 tipo = inp$tipo,
                 cobertura = inp$cobertura,
                 tematica = inp$tematica,
                 subregiones = subR,
                 with_parent = inp$with_parent,
                 con = con)
    d <- d |> sib_merge_ind_label(con = con)
    d

    # tryCatch({
    #   inp <- inputs()
    #   subR <- inp$subregiones
    #   req(actual_but$active)
    #   if (actual_but$active == "map") subR <- TRUE
    #
    #   d <- sibdata(inp$region,
    #                grupo = inp$grupo,
    #                tipo = inp$tipo,
    #                cobertura = inp$cobertura,
    #                tematica = inp$tematica,
    #                subregiones = subR,
    #                with_parent = inp$with_parent)
    #   if (actual_but$active != "map") {
    #     d <- d |> sib_merge_ind_label()
    #   } else {
    #     d <- d |> dplyr::select(label, count)
    #     d$label <- dplyr::recode(d$label, "San Sebastián de Mariquita" = "Mariquita",
    #                              "San Andrés de Tumaco" = "Tumaco",
    #                              "Santacruz" = "Santa cruz",
    #                              "El Tablón de Gómez" = "El Tablón",
    #                              "Güicán de la Sierra" = "Guican")
    #   }
    #   d
    # },
    # error = function(cond) {
    #   return()
    # })
  }


  sel_grupo <- reactive({
    if(input$sel_grupo_type == "biologico"){
      return(input$sel_grupo_biologico)
    } else {
      return(input$sel_grupo_interes)
    }
  })

  available_charts <- reactive({
    req(data())
    if(is.null(data())) return(c("Tabla" = "table"))
    dd <- data()
    if(nrow(dd) == 1){
      return(c("Tabla" = "table"))
    }
    c( "Mapa" = "map", "Torta"= "pie", "Dona" = "donut", "Treemap" = "treemap","Barras" = "bar", "Tabla"="table")
    c("Torta"= "pie", "Dona" = "donut", "Treemap" = "treemap","Barras" = "bar", "Tabla"="table")
  })


  hover_viz <- reactive({
    # req(available_charts())
    names(available_charts())
  })


  actual_but <- reactiveValues(active = NULL)

  observe({
    # req(available_charts())
    # if (is.null(available_charts())) return()
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
      data = dd
      # dataLabels_show = TRUE,
      # color_by = names(dd)[1],
      # legend_show = FALSE,
      # text_family = "Lato",
      # axis_line_y_size = 1,
      # axis_line_x_size = 1,
      # axis_line_color = "#dbd9d9",
      # border_weight = 0.2,
      # grid_y_color = "#dbd9d9",
      # grid_x_width = 0,
      # palette_colors = c("#5151f2", "#4ad3ac", "#ffd150", "#00afff", "#ffe0bb", "#f26330", "#163875")
    )

    if (actual_but$active == "map") {
      # region <- inputs()$region
      # opts$color_by <- NULL
      # opts$legend_show <- TRUE
      # opts$palette_colors <- rev(c("#f26330", "#f77e38", "#fb9745", "#feae56", "#ffc570", "#ffdb93", "#ffeec9"))
      # opts$map_name <- paste0("col_depto_", region)
      # if (region == "colombia") opts$map_name <- "col_departments"
      # opts$topo_fill_opacity <- 0.6
      # opts$max_topo_fill_opacity <- 0.8
      # opts$map_opacity <- 0.5
    }
    opts
  })


  l_viz <- reactive({
    req(vizOps())
    opts <- vizOps()
    sel_chart_type <- actual_but$active
    if (sel_chart_type == "table") return()
    viz <- paste0("hgmagic::hg_", sel_chart_type, "_CatNum")
    if (sel_chart_type == "map") viz <- "lfltmagic::lflt_choropleth_GnmNum"
    suppressWarnings(do.call(eval(parse(text=viz)), opts))
  })


  output$hgch_viz <- renderHighchart({
    req(l_viz())
    sel_chart_type <- actual_but$active
    if (sel_chart_type %in% c("table", "map")) return()
    l_viz()
  })

  # output$lflt_viz <- renderLeaflet({
  #   req(l_viz())
  #   sel_chart_type <- actual_but$active
  #   if (!sel_chart_type %in% c( "map")) return()
  #   l_viz()
  # })

  output$dt_sum <- renderDataTable({
    req(data())
    data()
  })

  output$viz <- renderUI({
    req(actual_but$active)
    if (actual_but$active == "table") {
      dataTableOutput("dt_sum")
    } else if (actual_but$active == "map") {
      # leafletOutput("lflt_viz", height = 600)
    } else {
      highchartOutput("hgch_viz", height = 600)
    }
  })


  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      # downloadImageUI("download_viz", dropdownLabel = "Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown")
    } else {
      #downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown")
    }
  })

  # downloadTableServer("dropdown_table", element = reactive(data_fin()), formats = c("csv", "xlsx", "json"))
  # downloadImageServer("download_viz", element = reactive(l_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  # downloadTableButtonServer("species_table", element = reactive(data_especies()), formats = c("csv", "xlsx", "json"))


}

shinyApp(ui, server)
