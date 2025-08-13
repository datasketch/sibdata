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
    }, ignoreNULL = FALSE)

    # Sync especies_total_estimadas input with reactive values
    observeEvent(input$especies_total_estimadas, {
      r$especies_total_estimadas <- input$especies_total_estimadas
      if (debug) message("🔄 ESPECIES TOTAL/ESTIMADAS CHANGED to: ", r$especies_total_estimadas)
    })

    # Sync amenazadas_categoria input with reactive values
    observeEvent(input$amenazadas_categoria, {
      r$amenazadas_categoria <- input$amenazadas_categoria
      if (debug) message("🔄 AMENAZADAS CATEGORIA CHANGED to: ", r$amenazadas_categoria)
    })

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

      # Check if amenazadas with total category selected
      is_amenazadas_total <- !is.null(r$sel_tematica) &&
                            grepl("amenazadas", r$sel_tematica) &&
                            !is.null(r$amenazadas_categoria) &&
                            r$amenazadas_categoria == "_total"

      # Determine available charts based on rules
      if ((!is.null(r$has_subtematica) && r$has_subtematica) || is_amenazadas_total) {
        # For tematicas with subtematicas OR amenazadas with total: ALL charts available
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
        message("✓ Is amenazadas total: ", is_amenazadas_total)
      }
    })

    # Output for conditional panel (chart selector visibility)
    output$show_chart_selector <- reactive({
      # Only show chart selector when inputs are ready
      if (debug) cat("🔍 VIZ: Checking if inputs_ready:", r$inputs_ready, "\n")
      if (!isTruthy(r$inputs_ready)) return(FALSE)
      return(TRUE)
    })
    outputOptions(output, "show_chart_selector", suspendWhenHidden = FALSE)

    # Render breadcrumb based on r values
    output$breadcrumb <- renderText({
      create_breadcrumb(r)
    })


        # Fetch main data
    observe({
      if (debug) cat("🔍 VIZ: Main data observer triggered, inputs_ready:", r$inputs_ready, "\n")
      req(r$inputs_ready)
      req(r$sel_region)
      req(r$chart_type)  # Need chart_type to determine subregiones

      # Set subregiones based on chart type
      use_subregiones <- r$chart_type == "map"

      if (debug) {
        message("🔄 FETCHING MAIN DATA:")
        message("- Region: ", r$sel_region)
        message("- Grupo: ", r$sel_grupo)
        message("- Tipo: ", r$sel_tipo)
        message("- Tematica: ", r$sel_tematica)
        message("- Indicador: ", r$indicador)
        message("- Chart type: ", r$chart_type)
        message("- Subregiones: ", use_subregiones)
      }

      d <- tryCatch(sibdata(
        region = r$sel_region,
        grupo = r$sel_grupo,
        tipo = r$sel_tipo,
        tematica = r$tematica,  # Fixed typo
        indicador = r$indicador,
        subregiones = use_subregiones, # TRUE for maps, FALSE for other charts
        with_parent = FALSE,
        con = con
      ), error = function(e){
        if (debug) message("❌ ERROR IN SIBDATA: ", e$message)
        NULL
      })

      r$main_data <- d

      if (debug) {
        if (!is.null(d)) {
          message("✅ Data fetched successfully: ", nrow(d), " rows")
        } else {
          message("❌ No data returned")
        }
      }
    })


    # Visualization output UI
    output$viz_output <- renderUI({
      req(r$chart_type)

      # Sys.sleep(0.5)

      switch(r$chart_type,
        # "map" = leaflet::leafletOutput(ns("map_viz"), height = 450),
        "table" = DT::dataTableOutput(ns("table_viz")),
        "pie" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
        "donut" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
        "bar" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
        "treemap" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
        h3("Tipo de gráfico no soportado todavía")
      )


    })




    # Map rendering - TEST WITH REAL DATA + GEOTABLE CONNECTION
    output$map_viz <- leaflet::renderLeaflet({
      req(r$inputs_ready)
      req(r$main_data)
      req(r$chart_type == "map")

      if (debug) {
        message("🗺️ TESTING WITH REAL DATA + GEOTABLE CONNECTION")
        message("- Data rows: ", nrow(r$main_data))
        message("- Region: ", r$sel_region)
        message("- Chart type: ", r$chart_type)
        message("- Conmap available: ", !is.null(r$conmap))
      }

            # Store real map data for modal
      r$map_data <- r$main_data
      r$current_chart_data <- r$main_data

      # Test accessing geotable connection and call choropleth_map
      tryCatch({
        if (!is.null(r$conmap)) {
          tables <- DBI::dbListTables(r$conmap)
          if (debug) message("✅ Geotable connection works, tables: ", length(tables))
        }

        # NOW TEST THE ACTUAL CHOROPLETH_MAP FUNCTION
        if (debug) message("🧪 TESTING choropleth_map() function...")

        # FIX: Use only r$conmap to avoid database connection conflicts
        if (debug) message("🔧 FIXED: Using only r$conmap connection to avoid conflicts")

        # Monitor tematica UI during map rendering
        if (debug) message("🔧 BEFORE choropleth_map: Checking if tematica UI exists")

        result <- choropleth_map(
          data = r$main_data,
          region = r$sel_region,
          tipo = r$sel_tipo,
          tematica = r$sel_tematica,
          indicador = r$indicador,
          grupo = r$sel_grupo,
          subregiones = TRUE,
          with_parent = FALSE,
          con = con,
          conmap = r$conmap
        )

        if (debug) message("🔧 AFTER choropleth_map: Map rendering completed")

        if (debug) message("✅ choropleth_map() completed successfully!")
        return(result)

      }, error = function(e) {
        if (debug) message("❌ ERROR with choropleth_map: ", e$message)
        # Fallback to basic map
        leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::setView(lng = -74.06, lat = 4.6, zoom = 6) %>%
          leaflet::addMarkers(lng = -74.06, lat = 4.6,
                             popup = paste("Error:", e$message))
      })
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

    # Highcharts rendering for pie, donut, bar, treemap
    output$hgch_viz <- highcharter::renderHighchart({
      req(r$main_data)
      req(r$chart_type %in% c("pie", "donut", "bar", "treemap"))

      if (debug) {
        message("📊 RENDERING HIGHCHART:")
        message("- Chart type: ", r$chart_type)
        message("- Data rows: ", nrow(r$main_data))
      }

      # Store chart data for modal
      r$chart_data <- r$main_data
      r$current_chart_data <- r$main_data

      # Validate chart data
      if(!validate_chart_data(r$main_data, r$chart_type)) {
        if (debug) message("❌ Chart data validation failed")
        return(NULL)
      }

      # Create chart using hgmagic
      tryCatch({
        result <- create_hgmagic_chart(r$chart_type, r$main_data, r, con)
        if (debug) message("✅ Highchart created successfully")
        return(result)
      }, error = function(e) {
        if (debug) message("❌ ERROR creating highchart: ", e$message)
        return(NULL)
      })
    })

    # Download controls UI
    output$descargas <- renderUI({
      req(r$chart_type)

      div(style = "display: flex; gap: 5px; justify-content: flex-end; align-items: center;",
          # Data button (for all chart types)
          if(r$chart_type == "map") {
            actionButton(ns("show_map_data"), "Ver datos del mapa",
                        class = "btn-sm btn-outline-info")
          } else if(r$chart_type == "table") {
            actionButton(ns("show_table_data"), "Ver datos de la tabla",
                        class = "btn-sm btn-outline-info")
          } else {
            actionButton(ns("show_chart_data"), "Ver datos del gráfico",
                        class = "btn-sm btn-outline-info")
          },
          # Chart download button (for highcharts only)
          if(r$chart_type %in% c("pie", "donut", "bar", "treemap")) {
            actionButton(ns("download_chart"), "Descargar gráfico",
                        class = "btn-sm btn-outline-secondary")
          }
      )
    })

    # Show map data modal
    observeEvent(input$show_map_data, {
      req(r$map_data)

      showModal(modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h5(paste("Indicador:", if(!is.null(r$indicador) && r$indicador != "" && !is.na(r$indicador)) {
            tools::toTitleCase(gsub("_", " ", r$indicador))
          } else "N/A")),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            style = "font-size: 1.5rem; font-weight: bold; line-height: 1; color: #000; text-shadow: 0 1px 0 #fff; opacity: 0.5; border: none; background: none;",
            HTML("&times;")
          )
        ),
        size = "l",
        div(
          div(style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
              downloadTableUI(ns("map_modal_download"),
                             dropdownLabel = "Descargar datos",
                             formats = c("csv", "xlsx", "json"),
                             display = "dropdown",
                             dropdownWidth = 200)
          ),
          DT::dataTableOutput(ns("map_data_table"))
        ),
        footer = NULL,
        easyClose = TRUE
      ))

      # Initialize download server after modal is shown
      downloadTableServer("map_modal_download",
                         element = reactive({
                           req(r$map_data)
                           r$map_data
                         }),
                         formats = c("csv", "xlsx", "json"),
                         file_prefix = "datos_mapa",
                         debug = debug)
    })

    # Show table data modal
    observeEvent(input$show_table_data, {
      req(r$main_data)

      showModal(modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span("Datos de la Tabla"),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            style = "font-size: 1.5rem; font-weight: bold; line-height: 1; color: #000; text-shadow: 0 1px 0 #fff; opacity: 0.5; border: none; background: none;",
            HTML("&times;")
          )
        ),
        size = "l",
        div(
          h5(paste("Tipo de visualización:", tools::toTitleCase(r$chart_type))),
          h6(paste("Región:", tools::toTitleCase(gsub("-", " ", r$sel_region)), "| Tipo:", tools::toTitleCase(r$sel_tipo))),
          if(!is.null(r$sel_tematica)) {
            h6(paste("Temática:", tools::toTitleCase(gsub("-", " ", r$sel_tematica))))
          },
          br(),
          div(style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
              downloadTableUI(ns("table_modal_download"),
                             dropdownLabel = "Descargar datos",
                             formats = c("csv", "xlsx", "json"),
                             display = "dropdown",
                             dropdownWidth = 200)
          ),
          DT::dataTableOutput(ns("table_data_table"))
        ),
        footer = NULL,
        easyClose = TRUE
      ))

      # Initialize download server after modal is shown
      downloadTableServer("table_modal_download",
                         element = reactive({
                           req(r$main_data)
                           r$main_data
                         }),
                         formats = c("csv", "xlsx", "json"),
                         file_prefix = "datos_tabla",
                         debug = debug)
    })

    # Show chart data modal
    observeEvent(input$show_chart_data, {
      req(r$main_data)

      showModal(modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span("Datos del Gráfico"),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            style = "font-size: 1.5rem; font-weight: bold; line-height: 1; color: #000; text-shadow: 0 1px 0 #fff; opacity: 0.5; border: none; background: none;",
            HTML("&times;")
          )
        ),
        size = "l",
        div(
          h5(paste("Tipo de gráfico:", tools::toTitleCase(r$chart_type))),
          h6(paste("Región:", tools::toTitleCase(gsub("-", " ", r$sel_region)), "| Tipo:", tools::toTitleCase(r$sel_tipo))),
          if(!is.null(r$sel_tematica)) {
            h6(paste("Temática:", tools::toTitleCase(gsub("-", " ", r$sel_tematica))))
          },
          br(),
          div(style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
              downloadTableUI(ns("chart_modal_download"),
                             dropdownLabel = "Descargar datos",
                             formats = c("csv", "xlsx", "json"),
                             display = "dropdown",
                             dropdownWidth = 200)
          ),
          DT::dataTableOutput(ns("chart_data_table"))
        ),
        footer = NULL,
        easyClose = TRUE
      ))

      # Initialize download server after modal is shown
      downloadTableServer("chart_modal_download",
                         element = reactive({
                           req(r$main_data)
                           r$main_data
                         }),
                         formats = c("csv", "xlsx", "json"),
                         file_prefix = "datos_grafico",
                         debug = debug)
    })

    # Render map data table in modal
    output$map_data_table <- DT::renderDataTable({
      req(r$map_data)

      # Select and format only relevant columns
      display_data <- r$map_data

      # Keep only label and the indicator column, remove slug_region and label_region
      cols_to_keep <- c("label")

      # Add the indicator column (find it dynamically)
      if (!is.null(r$indicador) && r$indicador %in% names(display_data)) {
        cols_to_keep <- c(cols_to_keep, r$indicador)
      } else {
        # If no specific indicator, keep all numeric columns except slug and label_region
        numeric_cols <- names(display_data)[sapply(display_data, is.numeric)]
        cols_to_keep <- c(cols_to_keep, numeric_cols)
      }

      # Remove duplicate columns and non-essential columns
      cols_to_exclude <- c("slug_region", "label_region")
      cols_to_keep <- cols_to_keep[!cols_to_keep %in% cols_to_exclude]
      cols_to_keep <- unique(cols_to_keep[cols_to_keep %in% names(display_data)])

      # Select only the relevant columns
      display_data <- display_data[, cols_to_keep, drop = FALSE]

      # Apply friendly column names
      names(display_data) <- gsub("_", " ", names(display_data))

      DT::datatable(
        display_data,
        rownames = FALSE,
        selection = 'none',
        escape = FALSE,
        options = list(
          dom = 'Bftsp',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          scrollX = TRUE,
          scrollY = "400px",
          pageLength = 15,
          searching = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#4ad3ac', 'color': '#ffffff'});",
            "}"
          )
        )
      )
    })

    # Render chart data table in modal
    output$chart_data_table <- DT::renderDataTable({
      req(r$main_data)

      # Use chart data directly
      display_data <- r$main_data

      DT::datatable(
        display_data,
        rownames = FALSE,
        selection = 'none',
        escape = FALSE,
        options = list(
          dom = 'Bftsp',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          scrollX = TRUE,
          scrollY = "400px",
          pageLength = 15,
          searching = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#4ad3ac', 'color': '#ffffff'});",
            "}"
          )
        )
      )
    })

    # Render table data table in modal
    output$table_data_table <- DT::renderDataTable({
      req(r$main_data)

      # Use table data directly
      display_data <- r$main_data

      DT::datatable(
        display_data,
        rownames = FALSE,
        selection = 'none',
        escape = FALSE,
        options = list(
          dom = 'Bftsp',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          scrollX = TRUE,
          scrollY = "400px",
          pageLength = 15,
          searching = TRUE,
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
      r$sel_tipo == "especies" && r$especies_total_estimadas == "total" ~ "especies_region_total",
      r$sel_tipo == "especies" && r$especies_total_estimadas == "estimadas" ~ "especies_region_estimadas",
      TRUE ~ "registros_region_total"
    )
  } else if (!is.null(r$sel_tematica) && grepl("amenazadas", r$sel_tematica)) {
    # Amenazadas tematica - return NULL when _total category is selected
    if (!is.null(amenazadas_categoria) && amenazadas_categoria == "_total") {
      indicador <- NULL
    } else {
      tem <- gsub("_total","", tematica)
      indicador <- glue::glue("{regs_or_esps}_{tem}{amenazadas_categoria}")
    }
  } else if (!is.null(r$sel_tematica) && grepl("cites", r$sel_tematica)) {
    # Cites tematica - return NULL for total categories
    if (r$sel_tematica == "cites") {
      indicador <- NULL  # Return NULL for "cites" (total)
    } else {
      indicador <- case_when(
        !grepl("_i", r$tematica) ~ glue::glue("{regs_or_esps}_{tematica}_total"),
        TRUE ~ glue::glue("{regs_or_esps}_{tematica}")
      )
    }
  } else if (!is.null(r$sel_tematica) && grepl("exoticas", r$sel_tematica)) {
    # Exóticas tematica - return NULL for total categories
    if (r$sel_tematica == "exoticas-total") {
      indicador <- NULL  # Return NULL for "exoticas-total" (total)
    } else {
      indicador <- glue::glue("{regs_or_esps}_{tematica}")
    }
  } else {
    # Other tematicas
    indicador <- glue::glue("{regs_or_esps}_{tematica}")
  }

  return(indicador)
}



