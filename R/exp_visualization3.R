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
    # Chart selector - show when inputs are ready
    div(style = "text-align: center; margin-bottom: 15px;",
        uiOutput(ns("chart_selector_container"))
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

    # # Sync amenazadas_categoria input with reactive values
    # observeEvent(input$amenazadas_categoria, {
    #   r$amenazadas_categoria <- input$amenazadas_categoria
    #   if (debug) message("🔄 AMENAZADAS CATEGORIA CHANGED to: ", r$amenazadas_categoria)
    # })

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

    # Handle chart selection directly (no module needed)
    observeEvent(input$chart_type, {
      if(!is.null(input$chart_type)) {
        old_chart <- r$chart_type
        r$chart_type <- input$chart_type
        if (debug) message("Chart type changed: ", old_chart, " -> ", input$chart_type)
      }
    })

    # Compute available charts based on tipo and tematica (from app.R lines 340-355)
    observe({
      # REMOVED req(r$inputs_ready) to avoid circular dependency and timing issues

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

    # Chart selector container - ALWAYS show with default values, no dependency on inputs_ready
    output$chart_selector_container <- renderUI({
      if (debug) cat("🔍 VIZ: Rendering chart selector (ALWAYS visible)\n")

      # SIMPLIFIED APPROACH: Create chart selector directly instead of using module
      # to avoid namespacing issues - similar to app2.R
      # cat("📊 VIZ: Creating chart selector directly\n")

      # All chart types available
      all_charts <- c("Mapa" = "map", "Torta" = "pie", "Dona" = "donut",
                      "Treemap" = "treemap", "Barras" = "bar", "Tabla" = "table")

      # Get available charts from reactive values, with fallback to default
      av_charts <- if (!is.null(r$available_charts) && length(r$available_charts) > 0) {
        r$available_charts
      } else {
        # Default to map and table when nothing is set yet
        c("Mapa" = "map", "Tabla" = "table")
      }

      # Set active chart (first available if current is not available)
      active_chart <- if(!is.null(r$chart_type) && r$chart_type %in% av_charts) {
        r$chart_type
      } else {
        av_charts[1]
      }

      # Update chart type in reactive values if it changed
      if(is.null(r$chart_type) || !r$chart_type %in% av_charts) {
        r$chart_type <- active_chart
        if (debug) message("Chart type automatically updated to: ", active_chart)
      }

      # cat("📊 VIZ: Creating buttonImageInput with active:", active_chart, "\n")

      # Create buttonImageInput directly
      shinyinvoer::buttonImageInput(
        inputId = ns('chart_type'),
        images = all_charts,
        highlightColor = "#09A274",
        button_width = 28,
        path = 'www/viz_icons',
        active = active_chart,
        layout = "flex",
        disabled = all_charts[!all_charts %in% av_charts]
      )
    })

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

      # Check if there's an error to display
      if (!is.null(r$viz_error)) {
        chart_output <- div(
          h4("Error en la visualización", style = "color: red;"),
          verbatimTextOutput(ns("error_display"))
        )
      } else {
        # Normal visualization output
        chart_output <- switch(r$chart_type,
          "map" = leaflet::leafletOutput(ns("map_viz"), height = 450),
          "table" = DT::dataTableOutput(ns("table_viz")),
          "pie" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
          "donut" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
          "bar" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
          "treemap" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
          h3("Tipo de gráfico no soportado todavía")
        )
      }

      # Return chart output only (debug2 is handled in main app UI)
      chart_output
    })




    # Error display output
    output$error_display <- renderText({
      req(r$viz_error)
      r$viz_error
    })

    # Debug info is now handled by exp_debug2 module

    # Map rendering - WITH PROPER ERROR HANDLING
    output$map_viz <- leaflet::renderLeaflet({
      req(r$inputs_ready)
      req(r$main_data)
      req(r$chart_type == "map")

      # Clear any previous errors
      r$viz_error <- NULL

      if (debug) {
        message("🗺️ RENDERING MAP WITH ERROR HANDLING")
        message("- Data rows: ", nrow(r$main_data))
        message("- Region: ", r$sel_region)
        message("- Chart type: ", r$chart_type)
        message("- Conmap available: ", !is.null(r$conmap))
      }

      # Store real map data for modal
      r$map_data <- r$main_data
      r$current_chart_data <- r$main_data

      # Try to render the map, but capture errors properly
      tryCatch({
        if (!is.null(r$conmap)) {
          tables <- DBI::dbListTables(r$conmap)
          if (debug) message("✅ Geotable connection works, tables: ", length(tables))
        }

        if (debug) message("🧪 CALLING choropleth_map() function...")

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

        if (debug) message("✅ choropleth_map() completed successfully!")
        return(result)

      }, error = function(e) {
        if (debug) message("❌ ERROR with choropleth_map: ", e$message)

        # Create detailed error message
        error_msg <- paste0(
          "ERROR MESSAGE:\n",
          e$message, "\n\n",
          "FUNCTION INPUTS:\n",
          "- region: ", r$sel_region, "\n",
          "- tipo: ", r$sel_tipo, "\n",
          "- tematica: ", r$sel_tematica, "\n",
          "- indicador: ", r$indicador, "\n",
          "- grupo: ", r$sel_grupo, "\n",
          "- subregiones: TRUE\n",
          "- with_parent: FALSE\n\n",
          "DATA INFORMATION:\n",
          if (!is.null(r$main_data)) {
            paste0(
              "- Data rows: ", nrow(r$main_data), "\n",
              "- Data columns: ", ncol(r$main_data), "\n",
              "- Column names: ", paste(names(r$main_data), collapse = ", "), "\n",
              "- Data glimpse:\n",
              paste(capture.output(utils::str(r$main_data)), collapse = "\n")
            )
          } else {
            "- No data available (r$main_data is NULL)"
          }
        )

        # Store error for display
        r$viz_error <- error_msg

        # Return NULL to trigger error display
        return(NULL)
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

      # Clear any previous errors when rendering highcharts
      if (r$chart_type %in% c("pie", "donut", "bar", "treemap")) {
        r$viz_error <- NULL
      }

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

        # Create error message for validation failure
        error_msg <- paste0(
          "ERROR MESSAGE:\n",
          "Chart data validation failed for chart type: ", r$chart_type, "\n\n",
          "FUNCTION INPUTS:\n",
          "- region: ", r$sel_region, "\n",
          "- tipo: ", r$sel_tipo, "\n",
          "- tematica: ", r$sel_tematica, "\n",
          "- indicador: ", r$indicador, "\n",
          "- grupo: ", r$sel_grupo, "\n",
          "- chart_type: ", r$chart_type, "\n\n",
          "DATA INFORMATION:\n",
          if (!is.null(r$main_data)) {
            paste0(
              "- Data rows: ", nrow(r$main_data), "\n",
              "- Data columns: ", ncol(r$main_data), "\n",
              "- Column names: ", paste(names(r$main_data), collapse = ", "), "\n",
              "- Data glimpse:\n",
              paste(capture.output(utils::str(r$main_data)), collapse = "\n")
            )
          } else {
            "- No data available (r$main_data is NULL)"
          }
        )

        r$viz_error <- error_msg
        return(NULL)
      }

      # Create chart using hgmagic
      tryCatch({
        result <- create_hgmagic_chart(r$chart_type, r$main_data, r, con)
        if (debug) message("✅ Highchart created successfully")
        return(result)
      }, error = function(e) {
        if (debug) message("❌ ERROR creating highchart: ", e$message)

        # Create detailed error message
        error_msg <- paste0(
          "ERROR MESSAGE:\n",
          e$message, "\n\n",
          "FUNCTION INPUTS:\n",
          "- region: ", r$sel_region, "\n",
          "- tipo: ", r$sel_tipo, "\n",
          "- tematica: ", r$sel_tematica, "\n",
          "- indicador: ", r$indicador, "\n",
          "- grupo: ", r$sel_grupo, "\n",
          "- chart_type: ", r$chart_type, "\n\n",
          "DATA INFORMATION:\n",
          if (!is.null(r$main_data)) {
            paste0(
              "- Data rows: ", nrow(r$main_data), "\n",
              "- Data columns: ", ncol(r$main_data), "\n",
              "- Column names: ", paste(names(r$main_data), collapse = ", "), "\n",
              "- Data glimpse:\n",
              paste(capture.output(utils::str(r$main_data)), collapse = "\n")
            )
          } else {
            "- No data available (r$main_data is NULL)"
          }
        )

        r$viz_error <- error_msg
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
      indicador <- glue::glue("{regs_or_esps}_{tematica}{amenazadas_categoria}")
    }
  } else if (!is.null(r$sel_tematica) && grepl("cites", r$sel_tematica)) {
    # Cites tematica - return NULL for total categories
    if (r$sel_tematica == "cites") {
      indicador <- NULL  # Return NULL for "cites" (total)
    } else {
      indicador <- glue::glue("{regs_or_esps}_{tematica}")
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



