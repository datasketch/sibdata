# exp_visualization.R
# Enhanced Visualization Module for SIB Data App (Phase 2)
# Integrates all chart types with dynamic controls


exp_visualization_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Chart selector first (matching original app header_right position)
    div(style = "display: flex; justify-content: center; margin-bottom: 15px;",
        div(
          class = 'first-container',
          exp_chart_selector_ui(ns("chart_selector"))
        )
    ),

    # Add CSS from original app for buttonImageInput styling
    tags$style(HTML("
      .buttons-group {
        display: flex !important;
        padding: 0px !important;
      }
      
      .buttons-group,.button-style {
        width: 30px !important;
        height: 30px !important;
      }
      
      .buttons-group .button-style.active-btn {
        padding: 0px !important;
        width: 30px !important;
      }
      
      .button-checkmark {
        display: none;
      }
    ")),

    # Type selector and data controls below
    div(style = "display: flex; justify-content: space-between;",
        div(style = "flex: 1;",
            radioButtons(ns("sel_tipo"), "Tipo",
                         c("Observaciones" = "registros",
                           "Especies" = "especies"))
        ),
        div(style = "flex: 1; text-align: right;",
            # Dynamic data controls (subcategories, total/estimadas)
            exp_data_controls_ui(ns("data_controls"))
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

exp_visualization_server <- function(id, r, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize sub-modules
    exp_chart_selector_server("chart_selector", r)
    exp_data_controls_server("data_controls", r)

    # Update reactive values when inputs change
    observeEvent(input$sel_tipo, {
      r$sel_tipo <- input$sel_tipo
    })

    # Render breadcrumb based on r values
    output$breadcrumb <- renderText({
      req(r$sel_region)

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
    })

    # Visualization output UI
    output$viz_output <- renderUI({
      req(r$chart_type)

      switch(r$chart_type,
        "map" = leaflet::leafletOutput(ns("map_viz"), height = 450),
        "table" = DT::dataTableOutput(ns("table_viz")),
        "pie" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
        "donut" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
        "bar" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
        "treemap" = highcharter::highchartOutput(ns("hgch_viz"), height = 450),
        div("Unsupported chart type")
      )
    })

    # Map rendering - purely reactive to r values
    output$map_viz <- leaflet::renderLeaflet({
      message("=== MAP RENDER CALLED ===")
      message("Current state:")
      message("- r$sel_region: ", r$sel_region)
      message("- r$sel_tipo: ", r$sel_tipo)
      message("- r$chart_type: ", r$chart_type)
      message("- r$main_data is null: ", is.null(r$main_data))
      if (!is.null(r$main_data)) {
        message("- r$main_data rows: ", nrow(r$main_data))
      }

      req(r$sel_region)
      message("✓ r$sel_region requirement met")

      req(r$sel_tipo)
      message("✓ r$sel_tipo requirement met")

      req(r$chart_type == "map")
      message("✓ r$chart_type == 'map' requirement met")

      req(r$main_data)
      message("✓ r$main_data requirement met")

      message("=== ALL REQUIREMENTS MET - Starting map render ===")
      message("Using data from r$main_data with ", nrow(r$main_data), " rows")

      # Call choropleth_map function from R/map.R directly with r values
      tryCatch({
        start_time <- Sys.time()
        message("Map render started at: ", start_time)

        # Get map connection
        message("Getting geotable connection...")
        conmap <- geotable::gt_con()
        message("✓ Got geotable connection")

        message("Calling choropleth_map with:")
        message("- Data rows: ", nrow(r$main_data))
        message("- Region: ", r$sel_region)
        message("- Tipo: ", r$sel_tipo)
        message("- Tematica: ", r$sel_tematica)
        message("- Indicador: ", r$indicador)

        # Store the data that will be used for the map
        r$map_data <- r$main_data

        # Debug the data structure before calling choropleth_map
        message("📊 DATA STRUCTURE DEBUG:")
        message("Column names: ", paste(names(r$main_data), collapse = ", "))
        message("First few rows:")
        if (nrow(r$main_data) > 0) {
          for(i in 1:min(3, nrow(r$main_data))) {
            row_data <- paste(r$main_data[i,], collapse = " | ")
            message("Row ", i, ": ", row_data)
          }
        }

        # Call the choropleth_map function with all necessary parameters
        message("Calling choropleth_map function...")
        result <- choropleth_map(
          data = r$main_data,
          region = r$sel_region,
          tipo = r$sel_tipo,
          tematica = r$sel_tematica,
          indicador = r$indicador,
          grupo = r$sel_grupo,
          subregiones = TRUE,  # Always TRUE for maps
          with_parent = FALSE,
          con = con,
          conmap = conmap
        )

        end_time <- Sys.time()
        duration <- difftime(end_time, start_time, units = "secs")
        message("✓ choropleth_map completed in ", round(duration, 2), " seconds")
        message("✓ Result is null: ", is.null(result))
        if (!is.null(result)) {
          message("✓ Result class: ", class(result)[1])
        }

        message("=== MAP RENDER SUCCESSFUL ===")
        return(result)
      }, error = function(e) {
        message("❌ ERROR in map rendering:")
        message("Error message: ", e$message)
        message("Error details: ", conditionMessage(e))
        message("Call stack: ", paste(capture.output(traceback()), collapse = "\n"))

        message("Returning fallback leaflet map...")
        # Return a simple leaflet map as fallback
        fallback <- leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::setView(lng = -74.06, lat = 4.6, zoom = 6)

        message("✓ Fallback map created")
        return(fallback)
      })
    })

    # Highcharts rendering for pie, donut, bar, treemap
    output$hgch_viz <- highcharter::renderHighchart({
      message("=== HIGHCHART RENDER CALLED ===")
      message("Chart type: ", r$chart_type)

      req(r$chart_type %in% c("pie", "donut", "bar", "treemap"))
      req(r$main_data)

      message("Creating hgmagic chart...")

      # Validate chart data
      if(!validate_chart_data(r$main_data, r$chart_type)) {
        message("❌ Chart data validation failed")
        return(NULL)
      }

      # Create chart using hgmagic
      tryCatch({
        result <- create_hgmagic_chart(r$chart_type, r$main_data, r, con)
        message("✓ hgmagic chart created")
        return(result)
      }, error = function(e) {
        message("❌ ERROR creating hgmagic chart: ", e$message)
        return(NULL)
      })
    })

    # Table rendering - purely reactive to r values
    output$table_viz <- DT::renderDataTable({
      req(r$main_data)
      req(r$chart_type == "table")

      d <- r$main_data

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

    # Download controls UI
    output$descargas <- renderUI({
      req(r$chart_type)

      div(style = "display: flex; gap: 5px; justify-content: flex-end; align-items: center;",
          # Map data button (only for maps)
          if(r$chart_type == "map") {
            actionButton(ns("show_map_data"), "Ver datos del mapa",
                        class = "btn-sm btn-outline-info")
          },
          # Chart data button (for charts)
          if(r$chart_type %in% c("pie", "donut", "bar", "treemap")) {
            actionButton(ns("show_chart_data"), "Ver datos del gráfico",
                        class = "btn-sm btn-outline-info")
          },
          # Chart download button (for highcharts)
          if(r$chart_type %in% c("pie", "donut", "bar", "treemap")) {
            actionButton(ns("download_chart"), "Descargar gráfico",
                        class = "btn-sm btn-outline-secondary")
          },
          # Download dropdown
          downloadTableUI(ns("dropdown_table"),
                         dropdownLabel = "Descargar datos",
                         formats = c("csv", "xlsx", "json"),
                         display = "dropdown")
      )
    })

    # Download table server
    message("🔧 Initializing download server with ID: ", ns("dropdown_table"))
    downloadTableServer("dropdown_table",
                       element = reactive({
                         message("📊 Download reactive called, main_data is null: ", is.null(r$main_data))
                         if (!is.null(r$main_data)) {
                           message("📊 Download reactive returning ", nrow(r$main_data), " rows")
                         }
                         r$main_data
                       }),
                       formats = c("csv", "xlsx", "json"),
                       file_prefix = "sibdata")

    # Show map data modal
    observeEvent(input$show_map_data, {
      req(r$map_data)

      showModal(modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span("Datos del Mapa"),
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
          h5(paste("Indicador:", sib_merge_ind_label(r$indicador, con = con))),
          h6(paste("Región:", tools::toTitleCase(gsub("-", " ", r$sel_region)), "| Tipo:", tools::toTitleCase(r$sel_tipo))),
          br(),
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

    # Download map data server for modal
    downloadTableServer("map_modal_download",
                       element = reactive({
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
                         names(display_data) <- sib_merge_ind_label(names(display_data), con = con)

                         display_data
                       }),
                       formats = c("csv", "xlsx", "json"),
                       file_prefix = "datos_mapa")

    # Render chart data table in modal
    output$chart_data_table <- DT::renderDataTable({
      req(r$main_data)

      # Use chart data directly (already processed with labels)
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

    # Download chart data server for modal
    downloadTableServer("chart_modal_download",
                       element = reactive({
                         req(r$main_data)
                         r$main_data
                       }),
                       formats = c("csv", "xlsx", "json"),
                       file_prefix = "datos_grafico")

  })
}
