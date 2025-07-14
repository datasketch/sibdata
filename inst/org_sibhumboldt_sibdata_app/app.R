
# remotes::install_github("datasketch/dsmods@3fc6c8")
# remotes::install_github("datasketch/shinyinvoer@57d98b")

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
library(shinydisconnect)


debug <- TRUE
debug <- FALSE



# UI ###############

ui <- panelsPage(
  disconnectMessage(
    text = "No fue posible hacer el cruce solicitado, si crees que este fue un error contáctanos",
    refresh = "Limpiar filtros",
    background = "#F0F0F0E4",
    colour = "#09A274",
    refreshColour = "#FFFFFF",
    overlayColour = "#FFFFFF",
    overlayOpacity = 0.39,
    width = "full",
    top = "center",
    size = 22,
    css = ""
  ),
  tags$style(HTML("#ss-connect-dialog a::before {
    background: #09A274 !important;
  }")),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),
  panel(title = "Opciones", width = 280,
        body = div(
          # actionButton("disconnect", "Good Bye!"),
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
          # uiOutput("debug_table"),
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
        body = div(
          # Add summary text above the table
          div(
            style = "margin-bottom: 15px; padding: 10px; background-color: #f5f5f5; border-radius: 4px;",
            textOutput("species_summary")
          ),
          dataTableOutput("list_species")
        )
  )
)

## SERVER ############

server <-  function(input, output, session) {



  dbdir <- "db/sibdata.sqlite"
  # dbdir <- "db/sibdata.duckdb"
  con <- DBI::dbConnect(RSQLite::SQLite(), dbdir, read_only = TRUE)
  # con <- duckdb_con(db)


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
  # opts_region <- c(
  #   opts_region,
  #   "Resguardo Pialapí Pueblo Viejo" = "resguardo-indigena-pialapi-pueblo-viejo",
  #   "Reserva Natural La Planada" = "reserva-natural-la-planada"
  # )


  opts_tematicas <- c(sib_available_tematicas(), "Ninguna" = "todas")
  opts_tematicas_ex <- c("cites_i", "cites_ii","cites_i_ii", "cites_iii",
                         "exoticas_total"
                         #"exoticas", "invasoras", "riesgo_invasion"
  )
  opts_tematicas <- opts_tematicas[!opts_tematicas %in% opts_tematicas_ex]
  # opts_tematicas <- gsub("_","-",opts_tematicas) # hay diferencia entre sibdata y list_species
  # uno recibe _ y el otro -








  conmap <- gt_con()

  r <- reactiveValues(
    amenazadas_categoria = NULL,
    cites_categoria = NULL,
    exotica_categoria = NULL,
    especies_total_estimadas = NULL,
    indicador = NULL,
    show_subcategoria = FALSE,
    show_especies_total_estimadas = FALSE,
    breadcrumb = NULL,
    current_subcategory = NULL
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
    if (!is.null(url_par()$grupo)){
      default_select <- tolower(url_par()$grupo)
      # Fix: Check both biological and interest groups to determine correct type
      group_type <- "biologico"  # default to biological
      # First check if it's in biological groups
      bio_exists <- sibdata_grupo(con) |>
        filter(slug == default_select, tipo == "biologico") |>
        collect() |>
        nrow() > 0

      if (!bio_exists) {
        # If not in biological groups, check interest groups
        int_exists <- default_select %in% av_grupos_int
        if (int_exists) {
          group_type <- "interes"
        }
      }

      updateRadioButtons(session,
                        inputId = "sel_grupo_type",
                        selected = group_type)
    }

    opts <- opts_grupo_interes

    list(
      conditionalPanel(
        condition = "input.sel_grupo_type == 'biologico'",
        selectInput("sel_grupo_bio",
                    "Seleccione grupo biológico",
                    opts_grupo_biologico, selected = default_select)
      ),
      conditionalPanel(
        condition = "input.sel_grupo_type != 'biologico'",
        selectInput("sel_grupo_int",
                    "Seleccione grupo de interés",
                    opts_grupo_interes, selected = default_select)
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
    # message("sel_tipo: ", input$sel_tipo)
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
    # message("show especies total estimada: ", show_especies_total_estimadas)
    # r$show_especies_total_estimadas <- ifelse(is.null(show_especies_total_estimadas), FALSE, TRUE)
    r$show_especies_total_estimadas <-show_especies_total_estimadas
  })



  #### DEBUG ######

  output$debug1 <- renderPrint({
    # str(input$sel_tematica)
    # str(input$sel_grupo_type)
    # str("GRUPO")
    # str(sel_grupo())
    # str(input$sel_grupo_bio)
    # str(input$sel_grupo_int)
    # str(input$chart_type)
    # str("INDICADOR")
    # str(r$indicador)
    #
    # str("IS AMENAZADAS CITES O EXÓTICAS")
    # str(is_amenazadas_or_cites_or_exoticas())
    # str("SHOW SUBCATEGORIA")
    # str(r$show_subcategoria)
    # str("SHOW ESPECIES TOTALES ESTIMADAS")
    # str(r$show_especies_total_estimadas)
    # str("CURRENT_CHART")
    #str(available_charts())
    # str(current_chart())
    # str("INPUTS")
    # str(inputs())
    # str("DATA_PARAMS")
    # str(data_params())
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
      # message("show esp 2", r$show_especies_total_estimadas)
      out <- tagList(out, selectInput("especies_total_estimadas", "Total o Estimadas",
                                      c("Total" = "total",
                                        "Estimadas" = "estimadas"
                                      )))
    }
    out
  })





  ## DATA PARAMS ######

  data_params <- reactive({
    message("==== data_params called ====")
    message("Current inputs: region=", input$sel_region,
            ", tematica=", input$sel_tematica,
            ", grupo_type=", input$sel_grupo_type,
            ", amenazadas_cat=", input$amenazadas_categoria,
            ", cites_cat=", input$cites_categoria)

    req(inputs())
    req(current_chart())
    input$chart_type
    inp <- inputs()
    tematica <- inp$tematica

    # Update indicador
    r$indicador <- NULL
    r$amenazadas_categoria <- NULL
    r$cites_categoria <- NULL
    r$especies_total_estimadas <- NULL

    # Actualizar indicador solo para los mapas
    # TODO show the table with
    if(current_chart() == "map" ){
      # caso amanazadas cites o exoticas
      if(is_amenazadas_or_cites_or_exoticas()){
        if(grepl("amenazadas", tematica)){
          if(!is.null(input$amenazadas_categoria)){
            r$indicador <- paste0(inputs()$tipo, "_", tematica, input$amenazadas_categoria)
            r$amenazadas_categoria <- input$amenazadas_categoria
          }
        }
        if(grepl("cites", tematica)){
          if(!is.null(input$cites_categoria)){
            r$indicador <- paste0(inputs()$tipo, "_", tematica, input$cites_categoria)
            r$cites_categoria <- input$cites_categoria
          }
        }
        # if(grepl("exoticas_total", input$sel_tematica)){
        #   indicador <- paste0(input$sel_tipo, "_", tematica, input$exoticas_categoria)
        # }
      }else{
        # Case for non amenazadas, cites, exóticas
        if(inputs()$tipo == "especies" && is.null(inputs()$tematica)){
          if(!is.null(input$especies_total_estimadas)){
            r$indicador <- paste0(input$sel_tipo, "_region_", input$especies_total_estimadas)
            r$especies_total_estimadas <- input$especies_total_estimadas
          }
        }

      }
      # Caso exóticas
      # Para ver los casos de exóticas
      if(is_exotica()){
        if(tematica %in% c("invasoras", "riesgo_invasion")) tematica <- "exoticas"
        indicador <-  paste0(input$sel_tipo, "_", inputs()$tematica)
        if(inputs()$tematica == "riesgo_invasion"){
          indicador <-  paste0(input$sel_tipo, "_exoticas_", inputs()$tematica)
        }
        r$indicador <- indicador
        r$exotica_categoria <- inputs()$tematica
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

    # At the end, print the results
    message("data_params returning: region=", inp$region,
            ", tipo=", inp$tipo,
            ", tematica=", tematica,
            ", indicador=", r$indicador)

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

  observe({
    req(data_params())
    req(data())
    tematica <- NULL
    # message("BREADCRUMS tematica: ", data_params()$tematica)
    if(!is.null(data_params()$tematica)){
      if(data_params()$tematica == "exoticas"){
        tematica <- r$exotica_categoria
        # message("  ", tematica)
      } else if(data_params()$tematica == "cites" && !is.null(input$cites_categoria)) {
        # Handle CITES categories the same way as in data_especies
        tematica <- switch(input$cites_categoria,
          "_total" = "cites",
          "_i" = "cites-i",
          "_ii" = "cites-ii",
          "_iii" = "cites-iii",
          "_i_ii" = "cites-i-ii",
          "cites"  # default case
        )
      } else {
        tematica <- data_params()$tematica
      }
    }
    # message("especies total estimadas: ", r$especies_total_estimadas)
    # message("amenazadas categoria: ", r$amenazadas_categoria)

    text <- dstools::collapse(
      data_params()$region, data_params()$tipo,
      data_params()$grupo,
      tematica,  # Now using the properly formatted CITES tematica
      r$amenazadas_categoria,
      r$cites_categoria,
      r$especies_total_estimadas,
      collapse = " | ")
    # message("BREADCRUMB: ", text)
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
    r$breadcrumb <- text
  })

  output$breadcrumb <- renderText({
    r$breadcrumb
  })



  ### DATA #########

  data <- reactive({
    message("==== data reactive called ====")

    if(is.null(data_params())) {
      message("data_params() is NULL, returning NULL")
      return()
    }

    params <- data_params()
    message("Calling sibdata with: region=", params$region,
            ", tematica=", params$tematica,
            ", indicador=", params$indicador)

    d <- tryCatch({
      do.call("sibdata", params)
    }, error = function(e) {
      message("ERROR in sibdata call: ", e$message)
      return(NULL)
    })

    message("sibdata returned ", nrow(d), " rows")

    if(current_chart() %in% c("pie", "donut", "treemap", "bar", "table")){
      message("Merging indicator labels")
      d <- d |> sib_merge_ind_label(con = con)
    }

    message("data reactive returning ", nrow(d), " rows")
    return(d)
  })




  ### DEBUG 2 #####
  output$debug2 <- renderPrint({
    # str(is_amenazadas_or_cites_or_exoticas())
    # str(current_chart())
    # str(data())
    # str(vizOps())
    # str(input$sel_tematica)
    #str(l_viz())
  })




  ### VIZ ###########




  vizOps <- reactive({
    message("==== vizOps called ====")
    message("Current chart type: ", current_chart())

    req(data_params())
    req(current_chart())
    req(data())

    dd <- data()
    params <- data_params()

    message("vizOps received data_params: region=", params$region,
            ", tematica=", params$tematica,
            ", indicador=", params$indicador)

    # Debug column names before standardization
    message("Original column names: ", paste(names(dd), collapse=", "))

    # Standardize column names for map
    if(current_chart() == "map" && !is.null(params$indicador)) {
      # Find the indicator column - it should be the last one
      indicator_col <- NULL
      for(colname in names(dd)) {
        if(colname == params$indicador || grepl(params$indicador, colname)) {
          indicator_col <- colname
          break
        }
      }

      if(!is.null(indicator_col)) {
        message("Found indicator column: ", indicator_col)
        # ADD the value column without removing the original
        dd$value <- dd[[indicator_col]]

        # Ensure we have count column if needed
        if(!"count" %in% names(dd)) {
          dd$count <- dd[[indicator_col]]
        }

        # Ensure we have indicador column
        if(!"indicador" %in% names(dd)) {
          dd$indicador <- rep(params$indicador, nrow(dd))
        }

        message("Standardized column names: ", paste(names(dd), collapse=", "))
      } else {
        message("WARNING: Could not find indicator column matching: ", params$indicador)
      }
    }

    palette <- NULL
    palette_numeric <- NULL
    color_by <- NULL

    # if(!is.null(r$inputs$tematica)){
    if(!is.null(params)){
      # if(grepl("amenazadas", r$inputs$tematica)){
      if(!is.null(params$tematica)){
        if(grepl("amenazadas", params$tematica)){
          # palette <- c("#FF0000", "#FFA500", "#FFFF00")
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
      color_palette_numeric = palette_numeric,
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
      # Always need indicator for map, otherwise it can return multiple rows
      # for a given geography
      ### not working if(is_amenazadas_or_cites_or_exoticas() && is.null(params$indicator)) return()
      opts$conmap <- conmap
    }else{
      opts$con <- NULL
    }

    # Add before the return
    message("vizOps is returning options with data rows: ", nrow(dd))
    if(current_chart() == "map") {
      message("Map options: region=", params$region,
              ", indicador=", params$indicador)
    }

    opts
  })




  l_viz <- reactive({
    # Add explicit print of dependency values
    message("==== l_viz dependencies ====")
    message("input$sel_region: ", input$sel_region)
    message("input$sel_tematica: ", input$sel_tematica)
    message("input$amenazadas_categoria: ", input$amenazadas_categoria)
    message("input$cites_categoria: ", input$cites_categoria)
    message("r$current_subcategory: ", r$current_subcategory)
    message("r$indicador: ", r$indicador)

    req(vizOps())
    req(current_chart())

    if(is.null(current_chart())) {
      message("Returning NULL, no chart type")
      return()
    }

    opts <- vizOps()
    message("l_viz received vizOps")

    if (current_chart() == "table") {
      message("Table requested, returning NULL")
      return()
    }

    viz <- paste0("hgmagic::hg_", current_chart(), "_CatNum")
    if (current_chart() == "map") {
      message("Creating map with: region=", opts$region,
              ", indicador=", opts$indicador)

      # Debug the data structure
      message("Map data structure:")
      message(paste(capture.output(str(opts$data)), collapse="\n"))

      # Check if the value column exists
      if("value" %in% names(opts$data)) {
        message("'value' column exists in data")
      } else {
        message("WARNING: 'value' column missing from data")
        message("Available columns: ", paste(names(opts$data), collapse=", "))
      }

      viz <- "choropleth_map"
    }

    message("Calling function: ", viz)
    result <- tryCatch({
      do.call(eval(parse(text=viz)), opts)
    }, error = function(e) {
      message("ERROR in l_viz: ", e$message)  # Print the actual error message
      message("Error details: ", paste(capture.output(print(e)), collapse="\n"))
      NULL
    })

    message("l_viz returning result: ", !is.null(result))
    return(result)
  })


  output$hgch_viz <- renderHighchart({
    if(is.null(current_chart())) return()
    req(l_viz())
    if (current_chart() %in% c("table", "map")) return()
    l_viz()
  })

  output$lflt_viz <- renderLeaflet({
    message("==== renderLeaflet called ====")
    message("Current chart type: ", current_chart())
    message("Current amenazadas_categoria: ", input$amenazadas_categoria)
    message("Current cites_categoria: ", input$cites_categoria)
    message("Current r$current_subcategory: ", r$current_subcategory)
    message("Current r$indicador: ", r$indicador)

    # Force reactivity on subcategory changes
    if (!is.null(r$current_subcategory)) {
      # This will invalidate the rendering when current_subcategory changes
      message("Using subcategory for invalidation: ", r$current_subcategory)
    }

    if(is.null(current_chart())) {
      message("No chart type, not rendering map")
      return()
    }

    if (current_chart() != "map") {
      message("Chart is not map, not rendering")
      return()
    }

    message("About to call l_viz() for map")
    result <- tryCatch({
      req(l_viz())
      l_viz()
    }, error = function(e) {
      message("ERROR in renderLeaflet: ", e$message)
      NULL
    })

    message("Map rendering complete, returning result: ", !is.null(result))
    return(result)
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

  # Create a reactive for the species list that depends on subcategories
  data_especies <- reactive({
    req(input$sel_grupo_type)
    req(input$sel_tematica)

    # Force reactivity on subcategory changes
    r$current_subcategory

    grupo <-  input$sel_grupo_bio
    if (input$sel_grupo_type == "interes") grupo <- input$sel_grupo_int
    req(grupo)
    if (grupo == "todos") grupo <- NULL

    # Handle the base tematica
    tematica <- gsub("_", "-", input$sel_tematica)
    if (tematica == "todas") tematica <- NULL

    # Debug messages for initial state
    message("=== Initial Parameters ===")
    message("Initial tematica: ", tematica)
    message("CITES categoria: ", input$cites_categoria)
    message("Amenazadas categoria: ", input$amenazadas_categoria)
    message("Grupo: ", grupo)
    message("Region: ", input$sel_region)

    # Handle subcategories
    if (!is.null(tematica)) {
        # Handle Amenazadas (both Nacional and Global)
        if (grepl("amenazadas", tematica) && !is.null(input$amenazadas_categoria)) {
            if (input$amenazadas_categoria != "_total") {
                subcategoria <- substr(input$amenazadas_categoria, 2, nchar(input$amenazadas_categoria))
                tematica <- paste0(tematica, "-", subcategoria)
            }
        }

        # Handle CITES
        if (tematica == "cites" && !is.null(input$cites_categoria)) {
            if (input$cites_categoria == "_total") {
                tematica <- "cites"
            } else {
                subcategoria <- substr(input$cites_categoria, 2, nchar(input$cites_categoria))
                subcategoria <- gsub("_", "-", subcategoria)
                tematica <- paste0("cites-", subcategoria)
            }
        }
    }

    message("\n=== Final Parameters for list_species ===")
    message("region = ", input$sel_region)
    message("grupo = ", grupo)
    message("tematica = ", tematica)

    # Call list_species with the final tematica
    l_s <- list_species(
        region = input$sel_region,
        grupo = grupo,
        tematica = tematica,
        con = con
    ) |>
        collect()

    message("\n=== Results ===")
    message("Number of rows returned: ", nrow(l_s))

    # Continue with the data transformation
    l_s <- l_s |>
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

    message("Final number of rows after transformation: ", nrow(l_s))
    l_s
  })

  # Update current_subcategory in the existing reactiveValues
  observe({
    message("==== subcategory observer called ====")
    message("Current inputs: amenazadas_cat=", input$amenazadas_categoria,
            ", cites_cat=", input$cites_categoria)

    old_value <- r$current_subcategory

    if (!is.null(input$amenazadas_categoria)) {
      r$current_subcategory <- input$amenazadas_categoria
      message("==== Should trigger map update? ====")
      message("Current chart: ", current_chart())
      message("Setting r$current_subcategory to: ", input$amenazadas_categoria)
      message("Current r$indicador: ", r$indicador)
    } else if (!is.null(input$cites_categoria)) {
      r$current_subcategory <- input$cites_categoria
      message("==== Should trigger map update? ====")
      message("Current chart: ", current_chart())
      message("Setting r$current_subcategory to: ", input$cites_categoria)
      message("Current r$indicador: ", r$indicador)
    } else {
      r$current_subcategory <- NULL
    }

    message("r$current_subcategory changed from ",
            ifelse(is.null(old_value), "NULL", old_value),
            " to ",
            ifelse(is.null(r$current_subcategory), "NULL", r$current_subcategory))
  })

  # Modify the list_species output
  output$list_species <- renderDataTable({
    req(data_especies())
    l_s <- data_especies()
    l_s2 <- data_especies()

    # Debug output
    message("\n=== Table Update ===")
    message("Current rows in table: ", nrow(l_s))
    message("Current subcategory: ", r$current_subcategory)

    l_s$GBIF <- ifelse(is.na(l_s$GBIF), "",
                       paste0("<a href='", l_s$GBIF, "'  target='_blank'>", "GBIF", "</a>"))
    l_s$CBC <- ifelse(is.na(l_s$CBC), "",
                      paste0("<a href='", l_s$CBC, "'  target='_blank'>", "CBC", "</a>"))

    message("DATA SPECIES L_S()")
    str(l_s2)
    DT::datatable(l_s2,
                  rownames = F,
                  selection = 'none',
                  escape = FALSE,
                  options = list(
                    dom = 'Bftsp',
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    scrollX = T,
                    fixedColumns = TRUE,
                    fixedHeader = TRUE,
                    searching = FALSE,
                    info = FALSE,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#4ad3ac', 'color': '#ffffff'});",
                      "}")
                  ))
  })

  # Update the species summary text
  output$species_summary <- renderText({
    message("==== species_summary output called ====")
    message("Current tematica: ", input$sel_tematica)
    message("Current amenazadas_categoria: ", input$amenazadas_categoria)
    message("Current r$current_subcategory: ", r$current_subcategory)

    # Force dependency on current_subcategory to ensure updates
    if (!is.null(r$current_subcategory)) {
      message("Using r$current_subcategory: ", r$current_subcategory)
    }

    result <- species_description()
    message("Rendered description: ", result)
    result
  })

  # Keep the downloadTableServer
  downloadTableServer("species_table", element = reactive(data_especies()), formats = c("csv", "xlsx", "json"))

  # Modify the species_description reactive to handle CITES I/II correctly
  species_description <- reactive({
    # Add debug prints
    message("==== species_description called ====")
    message("Current tematica: ", input$sel_tematica)
    message("Current amenazadas_categoria: ", input$amenazadas_categoria)
    message("Current r$current_subcategory: ", r$current_subcategory)

    req(data_especies())

    total <- nrow(data_especies())
    message("TOTAL ROWS in data_especies: ", total)

    # Print the actual data we're working with
    lsrows <- nrow(data_especies())
    message("List species row count: ", lsrows)

    region <- input$sel_region
    region <- ifelse(is.null(region), "todas las regiones", region)
    region <- tools::toTitleCase(gsub("-", " ", region))

    # Get the actual tematica used in data_especies with subcategories
    actual_tematica <- NULL
    if (!is.null(input$sel_tematica) && input$sel_tematica != "todas") {
      base_tematica <- gsub("_", "-", input$sel_tematica)
      if (grepl("amenazadas", input$sel_tematica) && !is.null(input$amenazadas_categoria)) {
        if (input$amenazadas_categoria != "_total") {
          subcategoria <- substr(input$amenazadas_categoria, 2, nchar(input$amenazadas_categoria))
          actual_tematica <- paste0(base_tematica, "-", subcategoria)
        } else {
          actual_tematica <- base_tematica
        }
      } else if (grepl("cites", input$sel_tematica) && !is.null(input$cites_categoria)) {
        if (input$cites_categoria != "_total") {
          subcategoria <- substr(input$cites_categoria, 2, nchar(input$cites_categoria))
          subcategoria <- gsub("_", "-", subcategoria)
          actual_tematica <- paste0("cites-", subcategoria)
        } else {
          actual_tematica <- base_tematica
        }
      } else {
        actual_tematica <- base_tematica
      }
    }
    message("Actual tematica used in data_especies: ", actual_tematica)

    # Handle tematica and subcategories for description
    tematica_text <- input$sel_tematica
    if (!is.null(input$sel_tematica) && input$sel_tematica != "todas") {
      tematica_text <- gsub("_", " ", input$sel_tematica)
      tematica_text <- tools::toTitleCase(tematica_text)

      # Add subcategory if present
      if (grepl("amenazadas", input$sel_tematica) && !is.null(input$amenazadas_categoria)) {
        if (input$amenazadas_categoria != "_total") {
          cat <- toupper(gsub("_", "", input$amenazadas_categoria))
          tematica_text <- paste(tematica_text, cat)
        }
      } else if (grepl("cites", input$sel_tematica) && !is.null(input$cites_categoria)) {
        if (input$cites_categoria != "_total") {
          # Special handling for CITES categories
          cat <- switch(input$cites_categoria,
                       "_i" = "I",
                       "_ii" = "II",
                       "_iii" = "III",
                       "_i_ii" = "I/II",
                       toupper(gsub("_", "", input$cites_categoria)))
          tematica_text <- paste(tematica_text, cat)
        }
      }
    } else {
      tematica_text <- "todas las temáticas"
    }
    message("Final tematica_text for description: ", tematica_text)

    # Add grupo if selected
    grupo_text <- ""
    if (!is.null(sel_grupo()) && sel_grupo() != "todos") {
      grupo <- tools::toTitleCase(gsub("-", " ", sel_grupo()))
      grupo_text <- paste("del grupo", grupo)
    }

    result <- sprintf("Mostrando %s especies para %s en %s %s",
                      format(total, big.mark = ","),
                      tematica_text,
                      region,
                      grupo_text)
    message("Final description: ", result)
    result
  })

  # Ensure the connection is closed when the session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
    gt_discon(conmap)
  })

  observeEvent(input$disconnect, {
    session$close()
  })

  # Add to observeEvent for chart_type changes
  observeEvent(input$chart_type, {
    message("==== chart_type changed ====")
    message("New chart type: ", input$chart_type)
  }, ignoreNULL = TRUE)

  # Add to observeEvent for region changes
  observeEvent(input$sel_region, {
    message("==== sel_region changed ====")
    message("New region: ", input$sel_region)
  }, ignoreNULL = TRUE)

  # Add to observeEvent for tematica changes
  observeEvent(input$sel_tematica, {
    message("==== sel_tematica changed ====")
    message("New tematica: ", input$sel_tematica)
  }, ignoreNULL = TRUE)

  # Add to observeEvent for amenazadas_categoria changes
  observeEvent(input$amenazadas_categoria, {
    message("==== amenazadas_categoria changed ====")
    message("New category: ", input$amenazadas_categoria)
  }, ignoreNULL = TRUE)

  # Add to observeEvent for cites_categoria changes
  observeEvent(input$cites_categoria, {
    message("==== cites_categoria changed ====")
    message("New category: ", input$cites_categoria)
  }, ignoreNULL = TRUE)

}

shinyApp(ui, server)
