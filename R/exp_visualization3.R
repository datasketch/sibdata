# exp_visualization3.R
# Enhanced Visualization Module for SIB Data App3 (Phase 3)
# Integrates all chart types with dynamic controls

#' Visualization UI Module
#'
#' Creates the center panel visualization with chart selector and dynamic controls
#'
#' @param id Module ID
#' @return UI elements for visualization
#' @export
exp_visualization3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Chart selector - conditional based on region type
    div(style = "text-align: center; margin-bottom: 15px;",
        conditionalPanel(
          condition = "output.show_chart_selector",
          ns = ns,
          exp_chart_selector_ui(ns("chart_selector"))
        )
    ),

    # Type selector and data controls below
    div(style = "display: flex; justify-content: space-between;",
        div(style = "flex: 1;",
            radioButtons(ns("sel_tipo"), "Tipo",
                         c("Observaciones" = "registros",
                           "Especies" = "especies"),
                         selected = "registros")
        ),
        div(style = "flex: 1; text-align: right;",
            # Data controls for especies total/estimadas and subtematicas
            uiOutput(ns("data_controls"))
        )
    ),

    hr(),
    # Breadcrumb and download row
    div(style = "display: flex; justify-content: space-between; align-items: center;",
        div(style = "flex: 3;", textOutput(ns("breadcrumb"))),
        div(style = "flex: 1; text-align: right;", uiOutput(ns("descargas")))
    ),
    br(),
    # Visualization area
    uiOutput(ns("viz_output")),

    # Modal for showing map data
    div(id = ns("map_data_modal"))
  )
}

#' Visualization Server Module
#'
#' Handles the server logic for visualization including chart rendering and data modals
#'
#' @param id Module ID
#' @param r Reactive values object
#' @param con Database connection
#' @param debug Boolean to control console debug output
#' @export
exp_visualization3_server <- function(id, r, con, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Sync tipo input with reactive values
    observeEvent(input$sel_tipo, {
      if (debug) message("🔄 TIPO INPUT CHANGED to: ", input$sel_tipo)
      r$sel_tipo <- input$sel_tipo
      if (debug) message("✓ r$sel_tipo updated to: ", r$sel_tipo)
      # Sync especies_total_estimadas input with reactive values
    }, ignoreNULL = FALSE)

    # Simple data controls as renderUI - especies total/estimadas and amenazadas
    output$data_controls <- renderUI({

      # Show especies total/estimadas selector when:
      # - tipo is Especies, tematica is NULL, is_special_region is FALSE
      show_especies_total <- (!is.null(r$sel_tipo) && r$sel_tipo == "especies") &&
        is.null(r$sel_tematica) &&
        (!is.null(r$is_special_region) && !r$is_special_region)

      # Show amenazadas selector when tematica contains "amenazadas"
      show_amenazadas <- !is.null(r$sel_tematica) && grepl("amenazadas", r$sel_tematica)

      if (debug) {
        message("🎛️ DATA CONTROLS RENDERING:")
        message("Show_especies_total: ", show_especies_total)
        message("Show_amenazadas: ", show_amenazadas)
        message("sel_tematica: ", r$sel_tematica)
      }

      # Return the appropriate control
      if (show_especies_total) {
        selectInput(ns("especies_total_estimadas"),
                    "Total o Estimadas",
                    choices = c("Total" = "total", "Estimadas" = "estimadas"),
                    selected = "total")
      } else if (show_amenazadas) {
        selectInput(ns("amenazadas_categoria"),
                    "Categoría Amenaza",
                    choices = c("Total amenazadas" = "_total",
                                "EN" = "_en",
                                "CR" = "_cr",
                                "VU" = "_vu"),
                    selected = "_total")
      } else {
        NULL
      }
    })


    # Create indicador
    observe({
      r$amenazadas_categoria <- input$amenazadas_categoria
      indicador <- calculate_indicador(r)
      r$indicador <- indicador

      if (debug) {
        message("🔧 INDICADOR UPDATED:")
        message("- sel_tipo: ", r$sel_tipo)
        message("- tematica: ", r$tematica)
        message("- amenazadas_categoria: ", r$amenazadas_categoria)
        message("- indicador: ", r$indicador)
      }
    })

    # Initialize chart selector module
    exp_chart_selector_server("chart_selector", r, debug = debug)

    # Compute available charts based on tipo and tematica (from app.R lines 340-355)
    observe({
      req(r$inputs_ready)

      if (debug) message("🎨 COMPUTING AVAILABLE CHARTS")

      # All chart types available
      all_charts <- c("Mapa" = "map", "Torta" = "pie", "Dona" = "donut",
                      "Treemap" = "treemap", "Barras" = "bar", "Tabla" = "table")
      map_table <- c("Mapa" = "map", "Tabla" = "table")
      map_table_bar <- c("Mapa" = "map", "Tabla" = "table", "Barras" = "bar")

      # Determine available charts based on rules
      if (!is.null(r$has_subtematica) && r$has_subtematica) {
        # For tematicas with subtematicas (amenazadas/cites/exoticas): ALL charts available
        r$available_charts <- all_charts
      } else {
        # Regular themes
        if (!is.null(r$sel_tipo) && r$sel_tipo == "registros") {
          r$available_charts <- map_table
        } else if (!is.null(r$sel_tipo) && r$sel_tipo == "especies") {
          r$available_charts <- map_table_bar
        } else {
          r$available_charts <- map_table  # default
        }
      }

      # Ensure current chart is available, default to map
      if (is.null(r$chart_type) || !r$chart_type %in% r$available_charts) {
        r$chart_type <- "map"  # Always default to map
        if (debug) message("✓ Chart type set to default: map")
      }

      if (debug) {
        message("✓ Available charts: ", paste(names(r$available_charts), collapse = ", "))
        message("✓ Current chart type: ", r$chart_type)
      }
    })

    # Output for conditional panel (chart selector visibility)
    output$show_chart_selector <- reactive({
      # Only show chart selector when inputs are ready
      if (!isTruthy(r$inputs_ready)) return(FALSE)
      return(TRUE)
    })
    outputOptions(output, "show_chart_selector", suspendWhenHidden = FALSE)

    # Render breadcrumb based on r values
    output$breadcrumb <- renderText({
      create_breadcrumb(r)
    })


    observe({
      req(r$sel_region)

      d <- tryCatch(sibdata(
        region = r$sel_region,
        grupo = r$sel_grupo,
        tipo = r$sel_tipo,
        tematica = r$seltematica,
        indicador = r$indicador,
        subregiones = TRUE, # Always TRUE for maps
        with_parent = FALSE,
        con = con
      ), error = function(e){
        message("ERROR IN SIBDATA", e$message)
        NULL
      })
      r$main_data <- d
      d
    })


    # Visualization output UI
    output$viz_output <- renderUI({
      req(r$chart_type)

      # Sys.sleep(0.5)

      # switch(r$chart_type,
      #   "map" = leaflet::leafletOutput(ns("map_viz"), height = 450),
      #   "table" = DT::dataTableOutput(ns("table_viz")),
      #   "pie" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
      #   "donut" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
      #   "bar" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
      #   "treemap" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
      #   div("Unsupported chart type")
      # )

      NULL
      # DT::dataTableOutput(ns("table_viz"))

    })




    # Map rendering - purely reactive to r values
    output$map_viz <- leaflet::renderLeaflet({

      result <- choropleth_map(
        data = r$main_data,
        region = r$sel_region,
        tipo = r$sel_tipo,
        tematica = r$sel_tematica,
        indicador = r$indicador,
        grupo = r$sel_grupo,
        subregiones = TRUE,  # Always TRUE for maps
        with_parent = FALSE,
        con = r$con,
        conmap = r$conmap
      )
      result
    })


    # Table rendering - purely reactive to r values
    output$table_viz <- DT::renderDataTable({
      req(r$main_data)
      req(r$chart_type == "table")

      d <- r$main_data

      # Store table data
      r$table_data <- d  # Specific for table
      r$current_chart_data <- d  # General current chart data

      if (debug) {
        message("📊 TABLE DATA STORED:")
        message("- Table data rows: ", nrow(r$current_chart_data))
        message("- Table data columns: ", paste(names(r$current_chart_data), collapse = ", "))
      }

      # Format column names for display
      display_data <- d
      names(display_data) <- sib_merge_ind_label(names(display_data), con = con)

      DT::datatable(
        display_data,
        rownames = FALSE,
        selection = 'none',
        escape = FALSE,
        options = list(
          dom = 'Bftsp',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          scrollX = TRUE,
          fixedColumns = TRUE,
          fixedHeader = TRUE,
          searching = TRUE,
          info = TRUE,
          pageLength = 15,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#4ad3ac', 'color': '#ffffff'});",
            "}"
          )
        )
      )
    })


  })
}



create_breadcrumb <- function(r){

  region <- tools::toTitleCase(gsub("-", " ", r$sel_region))
  tipo_text <- if (r$sel_tipo == "registros") "Observaciones" else "Especies"

  tematica_text <- if (is.null(r$sel_tematica)) {
    "todas las temáticas"
  } else {
    tools::toTitleCase(gsub("-", " ", r$sel_tematica))
  }

  grupo_text <- ""
  if (!is.null(r$sel_grupo)) {
    grupo <- tools::toTitleCase(gsub("-", " ", r$sel_grupo))
    grupo_text <- paste("del grupo", grupo)
  }

  breadcrumb <- paste(tipo_text, "para", tematica_text, "en", region, grupo_text)
  r$breadcrumb <- breadcrumb
  return(breadcrumb)
}


calculate_indicador <- function(r){
  regs_or_esps <- r$sel_tipo
  tematica <- if(!is.null(r$sel_tematica)){
    gsub("-", "_", r$sel_tematica)} else {r$sel_tematica}
  amenazadas_categoria <- r$amenazadas_categoria
  r$tematica <- tematica

  if(is.null(tematica)){
    # No tematica - use especies total/estimadas logic
    indicador <- case_when(
      r$sel_tipo == "especies" && input$especies_total_estimadas == "total" ~ "especies_region_total",
      r$sel_tipo == "especies" && input$especies_total_estimadas == "estimadas" ~ "especies_region_estimadas",
      TRUE ~ "registros_region_total"
    )
  } else if (!is.null(r$sel_tematica) && grepl("amenazadas", r$sel_tematica)) {
    # Amenazadas tematica - include category
    tem <- gsub("_total","", tematica)
    indicador <- glue::glue("{regs_or_esps}_{tem}{amenazadas_categoria}")
  } else if (!is.null(r$sel_tematica) && grepl("cites", r$sel_tematica)) {
    # Cites tematica - include category
    indicador <- case_when(
      !grepl("_i", r$tematica) ~ glue::glue("{regs_or_esps}_{tematica}_total"),
      TRUE ~ glue::glue("{regs_or_esps}_{tematica}")
    )
  } else if (!is.null(r$sel_tematica) && grepl("exoticas", r$sel_tematica)) {
    # Exóticas tematica - include category
    indicador <- glue::glue("{regs_or_esps}_{tematica}")
  } else {
    # Other tematicas
    indicador <- glue::glue("{regs_or_esps}_{tematica}")
  }
}



