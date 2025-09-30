# exp_visualization4.R
# Lean Visualization Module for SIB Data App4
# Focuses only on rendering visualizations
# Input controls, chart selector, tipo, breadcrumb, and indicador are handled by exp_viz_inputs

#' Visualization UI Module
#'
#' Creates the center panel visualization - download buttons and viz area only
#'
#' @param id Module ID
#' @return UI elements for visualization
#' @export
exp_visualization4_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Download row
    div(style = "display: flex; justify-content: flex-end; align-items: center;",
        uiOutput(ns("descargas"))
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
#' All input controls, chart selector, tipo, breadcrumb, and indicador handled by exp_viz_inputs
#'
#' @param id Module ID
#' @param r Reactive values object
#' @param con Database connection
#' @param loading_fns Optional list with show/hide functions for loading overlay
#' @param debug Boolean to control console debug output
#' @export
exp_visualization4_server <- function(id, r, con, loading_fns = NULL, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # NOTE: tipo, chart_type, indicador, breadcrumb, and available_charts
    # are all computed by exp_viz_inputs module - we just consume them here


    # Fetch main data
    # Use observe with explicit reactive reads to ensure it triggers on changes
    observe({
      # Explicitly read all reactive dependencies to ensure observer triggers
      sel_tipo <- r$sel_tipo
      sel_tematica <- r$sel_tematica
      sel_grupo <- r$sel_grupo
      sel_region <- r$sel_region
      chart_type <- r$chart_type
      # Read indicador AFTER other values to ensure it's computed
      indicador <- r$indicador

      if (debug) {
        message("🔍 VIZ: Main data observer triggered")
        message("  - sel_tipo: ", sel_tipo)
        message("  - sel_tematica: ", sel_tematica)
        message("  - indicador: ", indicador)
        message("  - chart_type: ", chart_type)
      }
      req(sel_region)
      req(chart_type)  # Need chart_type to determine subregiones

      # Show loading for data operations
      if (!is.null(loading_fns)) {
        loading_fns$show("Cargando datos...")
      }

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
        tematica = compute_tematica_slug(r),
        indicador = r$indicador,
        subregiones = use_subregiones, # TRUE for maps, FALSE for other charts
        with_parent = FALSE,
        con = con
      ), error = function(e){
        if (debug) message("❌ ERROR IN SIBDATA: ", e$message)
        NULL
      })

      r$main_data <- d
      # Add timestamp to force reactive invalidation
      r$data_timestamp <- Sys.time()

      if (debug) {
        if (!is.null(d)) {
          message("✅ Data fetched successfully: ", nrow(d), " rows")
        } else {
          message("❌ No data returned")
        }
      }

      ##  Hide loading after data is processed
      # if (!is.null(loading_fns)) {
      #   shinyjs::delay(100, loading_fns$hide())  # Small delay to ensure data is processed
      # }
    })


    # Visualization output UI
    output$viz_output <- renderUI({
      req(r$chart_type)

      # Helper to render a friendly no-data message
      render_no_data_box <- function(){
        div(style = "border: 1px solid #4ad3ac; background: #F2FBF8; color: #09A274; padding: 16px; border-radius: 8px; text-align: center;",
            div(style = "font-size: 18px; font-weight: 600;", "Los filtros no arrojaron resultados"),
            div(style = "font-size: 14px; margin-top: 6px;", "Por favor amplía la búsqueda con categorías más genéricas")
        )
      }

      # Detect no-data scenarios for any visualization type
      no_rows <- is.null(r$main_data) || (is.data.frame(r$main_data) && nrow(r$main_data) == 0)

      # Additional guard: if we have rows but every numeric value relevant to the
      # visualization is NA or 0, treat as no-data. Prefer the active indicador → count → any numeric.
      no_numeric_values <- FALSE
      if (!no_rows && is.data.frame(r$main_data)) {
        if (!is.null(r$indicador) && r$indicador %in% names(r$main_data)) {
          vals <- r$main_data[[r$indicador]]
          no_numeric_values <- all(is.na(vals) | vals == 0)
        } else if ("count" %in% names(r$main_data)) {
          v <- r$main_data$count
          no_numeric_values <- all(is.na(v) | v == 0)
        } else {
          numeric_cols <- names(r$main_data)[sapply(r$main_data, is.numeric)]
          if (length(numeric_cols) > 0) {
            # If all numeric columns are entirely NA or 0, consider it no-data
            no_numeric_values <- all(sapply(numeric_cols, function(nm) {
              v <- r$main_data[[nm]]
              all(is.na(v) | v == 0)
            }))
          }
        }
      }

      # Prefer showing a friendly no-data message over a raw error block
      if (no_rows || no_numeric_values) {
        chart_output <- render_no_data_box()
      } else if (!is.null(r$viz_error)) {
        # Keep error details available for debug modules, but avoid breaking the UI
        chart_output <- render_no_data_box()
      } else {
        # Normal visualization output
        chart_output <- switch(r$chart_type,
          "cards" = uiOutput(ns("cards_viz")),
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



    # Initialize map rendering (as a helper function, not a nested module)
    #exp_visualization4_map_server(output, r, con, debug = debug)


    # Map rendering - WITH PROPER ERROR HANDLING
    output$map_viz <- leaflet::renderLeaflet({
      # CRITICAL: Read timestamp FIRST to force re-render when data changes
      data_timestamp <- r$data_timestamp

      # Read ALL reactive dependencies BEFORE any req() calls
      sel_tipo <- r$sel_tipo
      sel_tematica <- r$sel_tematica
      indicador <- r$indicador
      sel_grupo <- r$sel_grupo
      sel_region <- r$sel_region
      main_data <- r$main_data

      # Now apply requirements
      req(main_data)
      req(sel_region)

      # Clear any previous errors
      r$viz_error <- NULL

      if (debug) {
        message("🗺️ RENDERING MAP")
        message("  - Data rows: ", nrow(main_data))
        message("  - Region: ", sel_region)
        message("  - Tipo: ", sel_tipo)
        message("  - Indicador: ", indicador)
        message("  - Chart type: ", r$chart_type)
        message("  - Conmap available: ", !is.null(r$conmap))
      }

      # Store real map data for modal
      r$map_data <- r$main_data
      r$current_chart_data <- r$main_data


      # leaflet() %>%
      #   addTiles() %>%
      #   setView(lng = -74.0721, lat = 4.7110, zoom = 12) %>%
      #   addMarkers(lng = -74.0721, lat = 4.7110, popup = "Hello from Bogotá!") %>%
      #   addLegend(
      #     position = "bottomright",
      #     colors = c("blue"),
      #     labels = c("Bogotá Marker"),
      #     title = r$sel_tipo
      #   )

      choropleth_map(
            data = main_data,
            region = sel_region,
            tipo = sel_tipo,
            tematica = sel_tematica,
            indicador = indicador,
            grupo = sel_grupo,
            subregiones = TRUE,
            with_parent = FALSE,
            con = con,
            conmap = r$conmap,
            debug = debug
          )

      # # Try to render the map, but capture errors properly
      # tryCatch({
      #   if (!is.null(r$conmap)) {
      #     tables <- DBI::dbListTables(r$conmap)
      #     if (debug) message("✅ Geotable connection works, tables: ", length(tables))
      #   }
      #
      #   if (debug) message("🧪 CALLING choropleth_map() function...")
      #
      #   result <- choropleth_map(
      #     data = r$main_data,
      #     region = r$sel_region,
      #     tipo = r$sel_tipo,
      #     tematica = r$sel_tematica,
      #     indicador = r$indicador,
      #     grupo = r$sel_grupo,
      #     subregiones = TRUE,
      #     with_parent = FALSE,
      #     con = con,
      #     conmap = r$conmap
      #   )
      #
      #   if (debug) message("✅ choropleth_map() completed successfully!")
      #   return(result)
      #
      # }, error = function(e) {
      #   if (debug) message("❌ ERROR with choropleth_map: ", e$message)
      #
      #   # Create detailed error message
      #   error_msg <- paste0(
      #     "ERROR MESSAGE:\n",
      #     e$message, "\n\n",
      #     "FUNCTION INPUTS:\n",
      #     "- region: ", r$sel_region, "\n",
      #     "- tipo: ", r$sel_tipo, "\n",
      #     "- tematica: ", r$sel_tematica, "\n",
      #     "- indicador: ", r$indicador, "\n",
      #     "- grupo: ", r$sel_grupo, "\n",
      #     "- subregiones: TRUE\n",
      #     "- with_parent: FALSE\n\n",
      #     "DATA INFORMATION:\n",
      #     if (!is.null(r$main_data)) {
      #       paste0(
      #         "- Data rows: ", nrow(r$main_data), "\n",
      #         "- Data columns: ", ncol(r$main_data), "\n",
      #         "- Column names: ", paste(names(r$main_data), collapse = ", "), "\n",
      #         "- Data glimpse:\n",
      #         paste(capture.output(utils::str(r$main_data)), collapse = "\n")
      #       )
      #     } else {
      #       "- No data available (r$main_data is NULL)"
      #     }
      #   )
      #
      #   # Store error for display
      #   r$viz_error <- error_msg
      #
      #   # Return NULL to trigger error display
      #   return(NULL)
      # })
    })





    # Cards rendering - show basic indicators (no temática) or all indicators for temática
    output$cards_viz <- renderUI({
      req(r$inputs_ready)
      req(r$sel_region)

      # Clear any previous errors for cards
      r$viz_error <- NULL

      # Styles
      card_css <- "display: flex; gap: 12px; justify-content: space-between; flex-wrap: wrap;"
      box_css_active <- "flex: 1; min-width: 180px; border: 1px solid #4ad3ac; border-radius: 8px; padding: 16px; background: #F2FBF8; box-shadow: 0 1px 2px rgba(0,0,0,0.05);"
      box_css_inactive <- "flex: 1; min-width: 180px; border: 1px solid #e6e6e6; border-radius: 8px; padding: 16px; background: #f7f7f7; box-shadow: 0 1px 2px rgba(0,0,0,0.03);"
      value_css_active <- "font-size: 28px; font-weight: 700; color: #09A274; margin: 0;"
      value_css_inactive <- "font-size: 28px; font-weight: 700; color: #999999; margin: 0;"
      label_css <- "font-size: 13px; color: #666666; margin: 0; margin-top: 6px;"

      # If no temática selected: show registros/especies totals
      if (is.null(r$sel_tematica)) {
        fetch_indicator_value <- function(ind_key) {
          d <- tryCatch({
            sibdata(
              region = r$sel_region,
              grupo = r$sel_grupo,
              tipo = if (grepl("^especies", ind_key)) "especies" else "registros",
              tematica = NULL,
              indicador = ind_key,
              subregiones = FALSE,
              with_parent = FALSE,
              con = con
            )
          }, error = function(e) {
            if (debug) message("❌ ERROR fetching ", ind_key, ": ", e$message)
            NULL
          })
          if (is.null(d) || !ind_key %in% names(d)) return(NA_real_)
          val <- suppressWarnings(as.numeric(d[[ind_key]][1]))
          if (is.na(val)) 0 else val
        }

        ind_regs <- "registros_region_total"
        ind_esps <- "especies_region_total"
        val_regs <- fetch_indicator_value(ind_regs)
        val_esps <- fetch_indicator_value(ind_esps)

        labels <- sib_merge_ind_label(c(ind_regs, ind_esps), con = con)
        if (!is.null(names(labels)) && all(names(labels) != "")) {
          label_regs <- labels[[ind_regs]]
          label_esps <- labels[[ind_esps]]
        } else {
          label_regs <- labels[1]
          label_esps <- labels[2]
        }

        # Replace any occurrences of "registros" with "observaciones" in card labels
        label_regs <- gsub("(?i)registros", "Observaciones", label_regs, perl = TRUE)
        label_esps <- gsub("(?i)registros", "Observaciones", label_esps, perl = TRUE)

        r$current_chart_data <- data.frame(
          indicador = c(ind_regs, ind_esps),
          etiqueta = unname(labels),
          valor = c(val_regs, val_esps),
          stringsAsFactors = FALSE
        )

        # Determine which tipo is active
        is_especies <- identical(r$sel_tipo, "especies")
        box1_style <- if (is_especies) box_css_inactive else box_css_active  # registros card
        val1_style <- if (is_especies) value_css_inactive else value_css_active
        box2_style <- if (is_especies) box_css_active else box_css_inactive  # especies card
        val2_style <- if (is_especies) value_css_active else value_css_inactive

        return(div(
          style = card_css,
          div(style = box1_style,
              p(style = val1_style, format(val_regs, big.mark = ",", scientific = FALSE)),
              p(style = label_css, label_regs)
          ),
          div(style = box2_style,
              p(style = val2_style, format(val_esps, big.mark = ",", scientific = FALSE)),
              p(style = label_css, label_esps)
          )
        ))
      }

      # When temática selected: fetch all relevant indicators (tidy) and render one card per indicator
      d <- tryCatch({
        sibdata(
          region = r$sel_region,
          grupo = r$sel_grupo,
          tipo = NULL,
          tematica = compute_tematica_slug(r),
          indicador = NULL,
          subregiones = FALSE,
          with_parent = FALSE,
          con = con
        )
      }, error = function(e){
        if (debug) message("❌ ERROR fetching cards (temática): ", e$message)
        NULL
      })

      if (is.null(d) || nrow(d) == 0) {
        r$current_chart_data <- NULL
        return(div(style = "border: 1px solid #4ad3ac; background: #F2FBF8; color: #09A274; padding: 16px; border-radius: 8px; text-align: center;",
                   div(style = "font-size: 18px; font-weight: 600;", "Los filtros no arrojaron resultados"),
                   div(style = "font-size: 14px; margin-top: 6px;", "Por favor amplía la búsqueda con categorías más genéricas")
        ))
      }

      # Summarize by indicator
      d2 <- d |>
        dplyr::group_by(indicador) |>
        dplyr::summarise(valor = sum(count, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(valor))

      # Do NOT filter by subcategory for cards; show all subcategories as cards

      inds <- d2$indicador
      labels_vec <- sib_merge_ind_label(inds, con = con)
      etiqueta <- as.character(labels_vec)
      # Replace any occurrences of "registros" with "observaciones" in card labels
      etiqueta <- gsub("(?i)registros", "observaciones", etiqueta, perl = TRUE)

      r$current_chart_data <- data.frame(
        indicador = inds,
        etiqueta = etiqueta,
        valor = d2$valor,
        stringsAsFactors = FALSE
      )

      # Determine subcategory matching pattern and whether it exists among indicators
      subcat_pattern <- NULL
      if (!is.null(r$sel_tematica) && grepl("amenazadas", r$sel_tematica)) {
        if (!is.null(r$amenazadas_categoria) && r$amenazadas_categoria != "_total") {
          subcat_pattern <- paste0(r$amenazadas_categoria, "$")
        }
      } else if (!is.null(r$sel_tematica) && grepl("cites", r$sel_tematica)) {
        if (!is.null(r$sel_subtematica) && nzchar(r$sel_subtematica)) {
          sub_slug <- gsub("-", "_", r$sel_subtematica)
          subcat_pattern <- paste0(sub_slug, "$")
        }
      }
      any_sub_present <- !is.null(subcat_pattern) && any(grepl(subcat_pattern, r$current_chart_data$indicador))

      boxes <- lapply(seq_len(nrow(r$current_chart_data)), function(i){
        val <- r$current_chart_data$valor[i]
        lab <- r$current_chart_data$etiqueta[i]
        ind_slug <- r$current_chart_data$indicador[i]
        ind_tipo <- if (grepl("^especies", ind_slug)) "especies" else "registros"

        # Active by tipo
        active_by_tipo <- identical(ind_tipo, r$sel_tipo)

        # Active by subcategory (when selected)
        active_by_subcat <- !is.null(subcat_pattern) && grepl(subcat_pattern, ind_slug)

        # Require subcategory match if selected and present; else fallback to tipo-only
        is_active <- if (!is.null(subcat_pattern) && any_sub_present) {
          active_by_tipo && active_by_subcat
        } else {
          active_by_tipo
        }

        div(style = if (is_active) box_css_active else box_css_inactive,
            p(style = if (is_active) value_css_active else value_css_inactive,
              format(val, big.mark = ",", scientific = FALSE)
            ),
            p(style = label_css, lab)
        )
      })

      do.call(div, c(list(style = card_css), boxes))
    })




    # Error display output
    output$error_display <- renderText({
      req(r$viz_error)
      r$viz_error
    })

    # Debug info is now handled by exp_debug2 module

    # Map rendering is now handled by exp_visualization4_map module


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

      # Format data and column names for display
      display_data <- d
      # Normalize region columns: keep only Region from label_region
      if ("label_region" %in% names(display_data)) {
        display_data$Región <- display_data$label_region
        display_data$label_region <- NULL
      }
      if ("slug_region" %in% names(display_data)) {
        display_data$slug_region <- NULL
      }
      if ("indicador" %in% names(display_data)) {
        display_data$indicador <- as.character(
          sib_merge_ind_label(as.character(display_data$indicador), con = con)
        )
      }
      names(display_data) <- sib_merge_ind_label(names(display_data), con = con)
      # Force friendly headers for common english columns
      if ("indicator" %in% names(display_data)) {
        names(display_data)[names(display_data) == "indicator"] <- "Indicador"
      }
      if ("count" %in% names(display_data)) {
        names(display_data)[names(display_data) == "count"] <- "Número"
      }

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

    # Download controls UI - Main download functionality like app.R
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
          # # Download buttons for charts and maps
          # if(r$chart_type %in% c("pie", "donut", "bar", "treemap")) {
          #   downloadButton(ns("download_chart"), "Descargar gráfico",
          #                 class = "btn-sm btn-outline-secondary")
          # } else if(r$chart_type == "map") {
          #   downloadButton(ns("download_map"), "Descargar mapa",
          #                 class = "btn-sm btn-outline-secondary")
          # }
      )
    })

    # Chart image download handler - for highcharter charts
    output$download_chart <- downloadHandler(
      filename = function() {
        chart_type_name <- tools::toTitleCase(r$chart_type)
        region_name <- gsub("-", "_", r$sel_region)
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("grafico_", chart_type_name, "_", region_name, "_", timestamp, ".png")
      },
      content = function(file) {
        if (debug) message("📊 DOWNLOADING CHART IMAGE: ", file)

        # Show loading indicator
        if (!is.null(loading_fns)) {
          loading_fns$show("Generando imagen del gráfico...")
        }

        # Create the chart using the same logic as the visualization
        tryCatch({
          # Validate that we have data and it's a highcharter chart type
          req(r$main_data)
          req(r$chart_type %in% c("pie", "donut", "bar", "treemap"))

          if (debug) {
            message("Creating chart for download:")
            message("- Chart type: ", r$chart_type)
            message("- Data rows: ", nrow(r$main_data))
          }

          # Create the chart using hgmagic (same as in the visualization)
          chart <- create_hgmagic_chart(r$chart_type, r$main_data, r, con)

          # Export the chart to PNG
          # Method 1: Try using webshot if available
          if (requireNamespace("webshot", quietly = TRUE)) {
            temp_html <- tempfile(fileext = ".html")
            htmlwidgets::saveWidget(chart, temp_html, selfcontained = TRUE)

            # Use webshot to convert HTML to PNG
            webshot::webshot(temp_html, file,
                            vwidth = 800, vheight = 600,
                            delay = 2)

            # Clean up temporary file
            unlink(temp_html)

          } else {
            # Method 2: Fallback - save as HTML and inform user
            html_file <- gsub("\\.png$", ".html", file)
            htmlwidgets::saveWidget(chart, html_file, selfcontained = TRUE)

            # Create a message file instead of PNG
            png(file, width = 800, height = 600)
            plot.new()
            text(0.5, 0.6, "Chart saved as HTML file", cex = 1.5, col = "blue")
            text(0.5, 0.4, paste("Location:", basename(html_file)), cex = 1.2, col = "darkblue")
            text(0.5, 0.3, "Open in browser to view", cex = 1, col = "gray")
            dev.off()
          }

          if (debug) message("✅ Chart downloaded successfully: ", file)

          # Hide loading indicator on success
          if (!is.null(loading_fns)) {
            loading_fns$hide()
          }

        }, error = function(e) {
          if (debug) message("❌ Error downloading chart: ", e$message)

          # Create a simple error image
          png(file, width = 800, height = 600)
          plot.new()
          text(0.5, 0.5, paste("Error generating chart:\n", e$message),
               cex = 1.2, col = "red")
          dev.off()

          # Hide loading indicator on error
          if (!is.null(loading_fns)) {
            loading_fns$hide()
          }
        })
      }
    )

    # Map image download handler - for leaflet maps
    output$download_map <- downloadHandler(
      filename = function() {
        region_name <- gsub("-", "_", r$sel_region)
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("mapa_", region_name, "_", timestamp, ".png")
      },
      content = function(file) {
        if (debug) message("🗺️ DOWNLOADING MAP IMAGE: ", file)

        # Show loading indicator
        if (!is.null(loading_fns)) {
          loading_fns$show("Generando imagen del mapa...")
        }

        tryCatch({
          # Validate that we have data and it's a map
          req(r$main_data)
          req(r$chart_type == "map")

          if (debug) {
            message("Creating map for download:")
            message("- Region: ", r$sel_region)
            message("- Data rows: ", nrow(r$main_data))
            message("- Indicador: ", r$indicador)
          }

          # Create the map using the same choropleth_map function
          map <- choropleth_map(
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

          # Export the map to PNG
          # Method 1: Try using webshot if available
          if (requireNamespace("webshot", quietly = TRUE)) {
            temp_html <- tempfile(fileext = ".html")
            htmlwidgets::saveWidget(map, temp_html, selfcontained = TRUE)

            # Use webshot to convert HTML to PNG with larger size for maps
            webshot::webshot(temp_html, file,
                            vwidth = 1000, vheight = 800,
                            delay = 3)  # Longer delay for maps to load

            # Clean up temporary file
            unlink(temp_html)

          } else {
            # Method 2: Fallback - save as HTML and inform user
            html_file <- gsub("\\.png$", ".html", file)
            htmlwidgets::saveWidget(map, html_file, selfcontained = TRUE)

            # Create a message file instead of PNG
            png(file, width = 1000, height = 800)
            plot.new()
            text(0.5, 0.6, "Map saved as HTML file", cex = 1.5, col = "blue")
            text(0.5, 0.4, paste("Location:", basename(html_file)), cex = 1.2, col = "darkblue")
            text(0.5, 0.3, "Open in browser to view", cex = 1, col = "gray")
            dev.off()
          }

          if (debug) message("✅ Map downloaded successfully: ", file)

          # Hide loading indicator on success
          if (!is.null(loading_fns)) {
            loading_fns$hide()
          }

        }, error = function(e) {
          if (debug) message("❌ Error downloading map: ", e$message)

          # Create a simple error image
          png(file, width = 1000, height = 800)
          plot.new()
          text(0.5, 0.5, paste("Error generating map:\n", e$message),
               cex = 1.2, col = "red")
          dev.off()

          # Hide loading indicator on error
          if (!is.null(loading_fns)) {
            loading_fns$hide()
          }
        })
      }
    )

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
                           req(r$current_chart_data)
                           r$current_chart_data
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
      req(r$current_chart_data)

      # Use current chart data (works for cards and charts)
      display_data <- r$current_chart_data
      # Normalize region columns: keep only Region from label_region
      if ("label_region" %in% names(display_data)) {
        display_data$Region <- display_data$label_region
        display_data$label_region <- NULL
      }
      if ("slug_region" %in% names(display_data)) {
        display_data$slug_region <- NULL
      }
      if ("indicador" %in% names(display_data)) {
        display_data$indicador <- as.character(
          sib_merge_ind_label(as.character(display_data$indicador), con = con)
        )
      }

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
      # Normalize region columns: keep only Region from label_region
      if ("label_region" %in% names(display_data)) {
        display_data$Region <- display_data$label_region
        display_data$label_region <- NULL
      }
      if ("slug_region" %in% names(display_data)) {
        display_data$slug_region <- NULL
      }
      if ("indicador" %in% names(display_data)) {
        display_data$indicador <- as.character(
          sib_merge_ind_label(as.character(display_data$indicador), con = con)
        )
      }

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

# Helper function - kept here since it's visualization-specific
compute_tematica_slug <- function(r){
  # Determine the tematica argument to pass to data functions
  # Use parent temática from selection; for amenazadas with category != _total,
  # still pass the parent temática (e.g., amenazadas_global/nacional) so data
  # includes all subcategories; indicator filters will narrow as needed.
  if (is.null(r$sel_tematica)) return(NULL)
  return(gsub("-", "_", r$sel_tematica))
}



