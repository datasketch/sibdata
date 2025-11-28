# app3.R
# Modular version of SIB Data App - Recreated from scratch

# Debug configuration
DEBUG_MODE <- FALSE  # Set to FALSE to hide debug output
options(timeout = 600)

library(shiny)
library(DT)
library(tidyverse)
library(sibdata)
library(data.tree)
library(htmlwidgets)
library(leaflet)
library(leaflet.extras)
library(openxlsx)
library(jsonlite)
library(highcharter)
library(hgmagic)
library(shinyinvoer)
library(shinyjs)

# Optional dependency for chart image export
if (requireNamespace("webshot", quietly = TRUE)) {
  library(webshot)
}

# Debug module is available from sibdata package

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css"),
    # Loading spinner CSS
    tags$style(HTML("
      .loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(255, 255, 255, 0.9);
        z-index: 1040;
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
      }

      .loading-overlay[style*='display: none'],
      .loading-overlay.hidden {
        z-index: -1 !important;
        pointer-events: none !important;
      }

      .modal {
        z-index: 1055 !important;
      }

      .modal-backdrop {
        z-index: 1050 !important;
      }

      .spinner {
        border: 4px solid #f3f3f3;
        border-top: 4px solid #09A274;
        border-radius: 50%;
        width: 50px;
        height: 50px;
        animation: spin 1s linear infinite;
      }

      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }

      .loading-text {
        margin-top: 20px;
        font-size: 16px;
        color: #09A274;
        font-weight: 500;
      }

      .section-loading {
        position: relative;
        opacity: 0.6;
      }

      .section-loading::after {
        content: '';
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        border: 3px solid #f3f3f3;
        border-top: 3px solid #09A274;
        border-radius: 50%;
        width: 30px;
        height: 30px;
        animation: spin 1s linear infinite;
        z-index: 1000;
      }
    "))
  ),

  # Global loading overlay
  div(id = "global-loading", class = "loading-overlay", style = "display: none;",
      div(class = "spinner"),
      div(id = "loading-text", class = "loading-text", "Cargando aplicación...")
  ),

  fluidRow(
    # Left column - Input controls (25%)
    column(3, style = "padding: 0 5px;",
           wellPanel(
             exp_debug_ui("debug"),
             h4("Opciones"),
             exp_inputs_ui("inputs")
           )
    ),

    # Center column - Visualization (50%)
    column(6, style = "padding: 0 5px;",
           wellPanel(
             h4(style = "margin-top: 5px; margin-bottom: 10px;", "Visualización"),
             exp_viz_inputs_ui("viz_inputs"),
             # Breadcrumb and download row
             div(style = "display: flex; justify-content: flex-end; margin-bottom: 5px; margin-top: 5px;",
                 uiOutput("descargas")
             ),
             # Dynamic chart output based on chart_type
             uiOutput("chart_output"),
             # Add debug2 below map when debug mode is on
             exp_debug2_ui("debug2")
           )
    ),

    # Right column - Species table (25%)
    column(3, style = "padding: 0 5px;",
           wellPanel(
             exp_species_table_ui("species")
           )
    )
  )
)

server <- function(input, output, session) {

  if (DEBUG_MODE) message("🚀 SERVER STARTING")

  # Show loading on app start
  shinyjs::show("global-loading")

  # Helper functions for loading management
  show_loading <- function(text = "Cargando...") {
    shinyjs::html("loading-text", text)
    shinyjs::show("global-loading")
  }

  hide_loading <- function() {
    shinyjs::hide("global-loading")
    # Also add hidden class to ensure it doesn't interfere
    shinyjs::runjs("$('#global-loading').addClass('hidden');")
  }

  # Create session-specific app options
  # con <- get_app_connection("db/sibdata.sqlite", debug = DEBUG_MODE)
  db <- "db/sibdata.duckdb"
  con <- get_app_connection(db, debug = DEBUG_MODE)
  app_options <- get_app_options(con, debug = DEBUG_MODE)
  app_options$con <- con
  conmap <- gt_con()


  # Create session-specific reactive values
  r <- reactiveValues(
    sel_region = NULL,
    sel_grupo_tipo = "biologico",
    sel_grupo = NULL,
    sel_tematica = NULL,
    tematica = NULL,
    sel_tipo = "registros",
    chart_type = "map",
    is_special_region = FALSE,
    has_subtematica = FALSE,
    inputs_ready = NULL,
    # Data controls
    amenazadas_categoria = NULL,
    cites_categoria = NULL,
    exotica_categoria = NULL,
    especies_total_estimadas = NULL,
    # Chart selector - include cards first by default
    available_charts = c("Tarjetas" = "cards", "Mapa" = "map", "Tabla" = "table"),
    indicador = NULL,
    breadcrumb = NULL,
    # Data storage for charts and modals
    main_data = NULL,
    map_data = NULL,
    table_data = NULL,
    chart_data = NULL,
    current_chart_data = NULL,
    data_timestamp = NULL,  # Timestamp to force reactive invalidation
    # Error handling
    viz_error = NULL,
    # Database connections
    con = con,
    conmap = conmap
  )



  # Initialize modules
  if (DEBUG_MODE) message("📦 INITIALIZING MODULES")
  exp_inputs_server("inputs", r, app_options, session, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Inputs module initialized")

  exp_viz_inputs_server("viz_inputs", r, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Viz inputs module initialized")

  # Dynamic chart output based on chart_type (optimized to prevent flickering)
  output$chart_output <- renderUI({
    chart_type <- isolate(r$chart_type)
    req(chart_type)

    if (DEBUG_MODE) message("📊 Rendering UI for chart_type: ", chart_type)

    # Use switch for better performance
    switch(chart_type,
      "map" = leaflet::leafletOutput("map_viz", height = 450),
      "cards" = uiOutput("cards_viz"),
      "table" = DT::dataTableOutput("table_viz"),
      "pie" = highcharter::highchartOutput("hgch_viz", height = 450),
      "donut" = highcharter::highchartOutput("hgch_viz", height = 450),
      "bar" = highcharter::highchartOutput("hgch_viz", height = 450),
      "treemap" = highcharter::highchartOutput("hgch_viz", height = 450),
      div(h3("Tipo de gráfico no soportado"))
    )
  }) |>
    shiny::bindEvent(r$chart_type)  # Only update when chart_type changes

  # Map rendering - directly in app4.R like app-inputs4.R
  output$map_viz <- leaflet::renderLeaflet({
    # Wait for inputs to be ready (includes tematica UI stabilization delay)
    req(r$inputs_ready)

    # Read chart_type first to ensure map re-renders when switching back
    chart_type <- r$chart_type
    req(chart_type == "map")  # Only render when chart type is map

    # Explicitly read ALL reactive dependencies at the start to ensure proper reactivity
    sel_region <- r$sel_region
    sel_tipo <- r$sel_tipo
    sel_tematica <- r$sel_tematica
    sel_grupo <- r$sel_grupo
    indicador <- r$indicador
    tematica <- r$tematica

    req(sel_region)

    if (DEBUG_MODE) {
      message("\n🗺️ MAP RENDER TRIGGERED")
      message("  - sel_tipo: ", sel_tipo)
      message("  - sel_region: ", sel_region)
      message("  - indicador: ", indicador)
      message("  - tematica: ", tematica)
    }

    # Fetch data
    d <- tryCatch({
      sibdata(
        region = sel_region,
        grupo = sel_grupo,
        tipo = sel_tipo,
        tematica = tematica,
        indicador = indicador,
        subregiones = TRUE,
        with_parent = FALSE,
        con = con
      )
    }, error = function(e) {
      if (DEBUG_MODE) message("❌ Error fetching data: ", e$message)
      return(NULL)
    })

    req(d)
    if (DEBUG_MODE) message("✅ Data fetched: ", nrow(d), " rows")

    # Store for other uses (isolate to prevent unnecessary re-renders)
    isolate({
      r$main_data <- d
    })

    # Render map
    choropleth_map(
      data = d,
      region = sel_region,
      tipo = sel_tipo,
      tematica = sel_tematica,
      indicador = indicador,
      grupo = sel_grupo,
      subregiones = TRUE,
      with_parent = FALSE,
      con = con,
      conmap = conmap,
      debug = DEBUG_MODE
    )
  })

  # Cards rendering - with highlighting logic from exp_visualization3
  output$cards_viz <- renderUI({
    req(r$inputs_ready)
    req(r$sel_region)

    # Read reactive dependencies
    sel_tipo <- r$sel_tipo
    sel_tematica <- r$sel_tematica
    sel_grupo <- r$sel_grupo
    sel_region <- r$sel_region
    tematica <- r$tematica
    amenazadas_categoria <- r$amenazadas_categoria
    sel_subtematica <- r$sel_subtematica

    if (DEBUG_MODE) {
      message("🃏 RENDERING CARDS")
      message("  - sel_tematica: ", sel_tematica)
      message("  - sel_subtematica: ", if(is.null(sel_subtematica)) "NULL" else sel_subtematica)
      message("  - tematica: ", if(is.null(tematica)) "NULL" else tematica)
      message("  - sel_tipo: ", sel_tipo)
    }

    # Card styles
    card_css <- "display: flex; gap: 12px; justify-content: space-between; flex-wrap: wrap;"
    box_css_active <- "flex: 1; min-width: 180px; border: 1px solid #4ad3ac; border-radius: 8px; padding: 16px; background: #F2FBF8; box-shadow: 0 1px 2px rgba(0,0,0,0.05);"
    box_css_inactive <- "flex: 1; min-width: 180px; border: 1px solid #e6e6e6; border-radius: 8px; padding: 16px; background: #f7f7f7; box-shadow: 0 1px 2px rgba(0,0,0,0.03);"
    value_css_active <- "font-size: 28px; font-weight: 700; color: #09A274; margin: 0;"
    value_css_inactive <- "font-size: 28px; font-weight: 700; color: #999999; margin: 0;"
    label_css <- "font-size: 13px; color: #666666; margin: 0; margin-top: 6px;"

    # If no temática selected: show registros/especies totals
    if (is.null(sel_tematica)) {
      fetch_indicator_value <- function(ind_key) {
        d <- tryCatch({
          sibdata(
            region = sel_region,
            grupo = sel_grupo,
            tipo = if (grepl("^especies", ind_key)) "especies" else "registros",
            tematica = NULL,
            indicador = ind_key,
            subregiones = FALSE,
            with_parent = FALSE,
            con = con
          )
        }, error = function(e) {
          if (DEBUG_MODE) message("❌ ERROR fetching ", ind_key, ": ", e$message)
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

      # Replace registros with observaciones
      label_regs <- gsub("(?i)registros", "Observaciones", label_regs, perl = TRUE)
      label_esps <- gsub("(?i)registros", "Observaciones", label_esps, perl = TRUE)

      isolate({
        r$current_chart_data <- data.frame(
          indicador = c(ind_regs, ind_esps),
          etiqueta = unname(labels),
          valor = c(val_regs, val_esps),
          stringsAsFactors = FALSE
        )
      })

      # Determine which tipo is active
      is_especies <- identical(sel_tipo, "especies")
      box1_style <- if (is_especies) box_css_inactive else box_css_active
      val1_style <- if (is_especies) value_css_inactive else value_css_active
      box2_style <- if (is_especies) box_css_active else box_css_inactive
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

    # When temática selected: fetch ALL indicators using all_indicators = TRUE
    # This returns all subcategory indicators (e.g., for exoticas: especies_exoticas,
    # especies_invasoras, especies_trasplantadas, etc.)

    d <- tryCatch({
      sibdata(
        region = sel_region,
        grupo = sel_grupo,
        tematica = tematica,  # Use the computed tematica slug
        all_indicators = TRUE,  # CRITICAL: Get all subcategory indicators
        subregiones = FALSE,
        con = con
      )
    }, error = function(e){
      if (DEBUG_MODE) message("❌ ERROR fetching cards (temática): ", e$message)
      NULL
    })

    if (DEBUG_MODE && !is.null(d)) {
      message("🃏 Cards data fetched:")
      message("  - Rows: ", nrow(d))
      message("  - Indicators: ", paste(unique(d$indicador), collapse = ", "))
    }

    if (is.null(d) || nrow(d) == 0) {
      isolate({
        r$current_chart_data <- NULL
      })
      return(div(style = "border: 1px solid #4ad3ac; background: #F2FBF8; color: #09A274; padding: 16px; border-radius: 8px; text-align: center;",
                 div(style = "font-size: 18px; font-weight: 600;", "Los filtros no arrojaron resultados"),
                 div(style = "font-size: 14px; margin-top: 6px;", "Por favor amplía la búsqueda con categorías más genéricas")
      ))
    }

    # Filter out ambiente indicators (marinas, continentales, salobres)
    d <- d |> dplyr::filter(!grepl("marinas|continentales|salobres", indicador))

    # Sort: registros first, then especies, maintaining subcategory order
    # Extract base indicator name (without registros_/especies_ prefix)
    d <- d |>
      dplyr::mutate(
        tipo_order = ifelse(grepl("^especies_", indicador), 2, 1),
        base_indicator = gsub("^(registros_|especies_)", "", indicador)
      ) |>
      dplyr::arrange(base_indicator, tipo_order) |>
      dplyr::select(-tipo_order, -base_indicator)

    # Data is already aggregated with all_indicators = TRUE
    # Extract the values we need
    inds <- d$indicador
    labels_vec <- sib_merge_ind_label(inds, con = con)
    etiqueta <- as.character(labels_vec)
    etiqueta <- gsub("(?i)registros", "observaciones", etiqueta, perl = TRUE)

    isolate({
      r$current_chart_data <- data.frame(
        indicador = inds,
        etiqueta = etiqueta,
        valor = d$count,
        stringsAsFactors = FALSE
      )
    })

    # Determine subcategory matching pattern
    subcat_pattern <- NULL
    if (!is.null(sel_tematica) && grepl("amenazadas", sel_tematica)) {
      if (!is.null(amenazadas_categoria) && amenazadas_categoria != "_total") {
        subcat_pattern <- paste0(amenazadas_categoria, "$")
      }
    } else if (!is.null(sel_tematica) && grepl("cites", sel_tematica)) {
      if (!is.null(sel_subtematica) && nzchar(sel_subtematica)) {
        sub_slug <- gsub("-", "_", sel_subtematica)
        subcat_pattern <- paste0(sub_slug, "$")
      }
    } else if (!is.null(sel_tematica) && grepl("exoticas", sel_tematica)) {
      # Handle exoticas subcategories
      if (!is.null(sel_subtematica) && nzchar(sel_subtematica)) {
        sub_slug <- gsub("-", "_", sel_subtematica)
        subcat_pattern <- paste0(sub_slug, "$")
      }
    }
    any_sub_present <- !is.null(subcat_pattern) && any(grepl(subcat_pattern, r$current_chart_data$indicador))

    # Optimize: use purrr::map instead of lapply
    chart_data <- r$current_chart_data
    boxes <- purrr::map(seq_len(nrow(chart_data)), function(i) {
      val <- chart_data$valor[i]
      lab <- chart_data$etiqueta[i]
      ind_slug <- chart_data$indicador[i]
      ind_tipo <- if (grepl("^especies", ind_slug)) "especies" else "registros"

      # Active by tipo
      active_by_tipo <- identical(ind_tipo, sel_tipo)

      # Active by subcategory (when selected)
      active_by_subcat <- !is.null(subcat_pattern) && grepl(subcat_pattern, ind_slug)

      # Require subcategory match if selected and present; else fallback to tipo-only
      is_active <- if (!is.null(subcat_pattern) && any_sub_present) {
        active_by_tipo && active_by_subcat
      } else {
        active_by_tipo
      }

      div(
        style = if (is_active) box_css_active else box_css_inactive,
        p(
          style = if (is_active) value_css_active else value_css_inactive,
          format(val, big.mark = ",", scientific = FALSE)
        ),
        p(style = label_css, lab)
      )
    })

    do.call(div, c(list(style = card_css), boxes))
  })

  # Table rendering - with full formatting from exp_visualization3
  output$table_viz <- DT::renderDataTable({
    req(r$inputs_ready)

    # Read reactive dependencies
    sel_tipo <- r$sel_tipo
    sel_tematica <- r$sel_tematica
    indicador <- r$indicador
    sel_grupo <- r$sel_grupo
    sel_region <- r$sel_region
    tematica <- r$tematica

    req(sel_region)

    if (DEBUG_MODE) message("📊 RENDERING TABLE")

    # Fetch data
    d <- tryCatch({
      sibdata(
        region = sel_region,
        grupo = sel_grupo,
        tipo = sel_tipo,
        tematica = tematica,
        indicador = indicador,
        subregiones = TRUE,
        with_parent = FALSE,
        con = con
      )
    }, error = function(e) {
      if (DEBUG_MODE) message("❌ Error fetching table data: ", e$message)
      return(NULL)
    })

    req(d)

    # Store for other uses (isolate to prevent unnecessary re-renders)
    isolate({
      r$main_data <- d
      r$table_data <- d
      r$current_chart_data <- d
    })

    # Format data and column names for display
    display_data <- d
    # Normalize region columns
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
    # Force friendly headers for common columns
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
        language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json'),
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
    req(r$inputs_ready)

    # Read reactive dependencies
    sel_tipo <- r$sel_tipo
    sel_tematica <- r$sel_tematica
    indicador <- r$indicador
    sel_grupo <- r$sel_grupo
    sel_region <- r$sel_region
    tematica <- r$tematica
    chart_type <- r$chart_type

    req(sel_region)
    req(chart_type %in% c("pie", "donut", "bar", "treemap"))

    if (DEBUG_MODE) {
      message("📊 RENDERING HIGHCHART:")
      message("  - Chart type: ", chart_type)
    }

    # Fetch data - use subregiones = FALSE for charts (not map)
    d <- tryCatch({
      sibdata(
        region = sel_region,
        grupo = sel_grupo,
        tipo = sel_tipo,
        tematica = tematica,
        indicador = indicador,
        subregiones = FALSE,  # FALSE for charts, TRUE only for maps
        with_parent = FALSE,
        con = con
      )
    }, error = function(e) {
      if (DEBUG_MODE) message("❌ Error fetching chart data: ", e$message)
      return(NULL)
    })

    req(d)

    # Store for other uses (isolate to prevent unnecessary re-renders)
    isolate({
      r$main_data <- d
      r$chart_data <- d
      r$current_chart_data <- d
    })

    # Create chart using hgmagic
    tryCatch({
      result <- create_hgmagic_chart(chart_type, d, r, con)
      if (DEBUG_MODE) message("✅ Highchart created successfully")
      return(result)
    }, error = function(e) {
      if (DEBUG_MODE) message("❌ ERROR creating highchart: ", e$message)
      return(NULL)
    })
  })

  # Download button UI - show data modal button based on chart type (optimized)
  output$descargas <- renderUI({
    chart_type <- isolate(r$chart_type)
    req(chart_type)

    switch(chart_type,
      "map" = actionButton("show_map_data", "Ver datos del mapa",
                          class = "btn-sm btn-outline-info"),
      "table" = actionButton("show_table_data", "Ver datos de la tabla",
                            class = "btn-sm btn-outline-info"),
      "cards" = actionButton("show_cards_data", "Ver datos de las tarjetas",
                           class = "btn-sm btn-outline-info"),
      actionButton("show_chart_data", "Ver datos del gráfico",
                  class = "btn-sm btn-outline-info")
    )
  }) |>
    shiny::bindEvent(r$chart_type)  # Only update when chart_type changes

  # Show map data modal
  observeEvent(input$show_map_data, {
    req(r$main_data)

    # Ensure loading overlay is hidden before showing modal
    hide_loading()

    # Use a small delay to ensure DOM is ready
    shinyjs::delay(100, {
      showModal(modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h5(paste("Indicador:", if(!is.null(r$indicador) && r$indicador != "" && !is.na(r$indicador)) {
            tools::toTitleCase(gsub("_", " ", r$indicador))
          } else "Mapa")),
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
        DT::dataTableOutput("map_data_table"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    })
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
      DT::dataTableOutput("table_data_table"),
      footer = NULL,
      easyClose = TRUE
    ))
  })

  # Show cards data modal
  observeEvent(input$show_cards_data, {
    req(r$current_chart_data)

    showModal(modalDialog(
      title = div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        span("Datos de las Tarjetas"),
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
      DT::dataTableOutput("cards_data_table"),
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
      DT::dataTableOutput("chart_data_table"),
      footer = NULL,
      easyClose = TRUE
    ))
  })

  # Render map data table for modal (initial render, will be updated in observeEvent)
  output$map_data_table <- DT::renderDataTable({
    # Read reactive dependencies to ensure table updates when they change
    sel_tipo <- r$sel_tipo
    indicador <- r$indicador
    main_data <- r$main_data

    req(main_data)

    # Validate that main_data is a data frame with at least one row
    if (!is.data.frame(main_data) || nrow(main_data) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay datos disponibles"),
        rownames = FALSE,
        options = list(dom = 't', language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json'))
      ))
    }

    display_data <- main_data

    # Get available column names
    available_cols <- names(display_data)

    # Keep only label and indicator columns that actually exist
    cols_to_keep <- character(0)

    # Add label if it exists
    if ("label" %in% available_cols) {
      cols_to_keep <- c(cols_to_keep, "label")
    }

    # Add indicator if it exists
    if (!is.null(indicador) && indicador %in% available_cols) {
      cols_to_keep <- c(cols_to_keep, indicador)
    } else {
      # If indicator doesn't exist, get numeric columns (optimized with dplyr)
      numeric_cols <- display_data |>
        dplyr::select(dplyr::where(is.numeric)) |>
        names()
      cols_to_keep <- c(cols_to_keep, numeric_cols)
    }

    # Exclude unwanted columns
    cols_to_exclude <- c("slug_region", "label_region")
    cols_to_keep <- cols_to_keep[!cols_to_keep %in% cols_to_exclude]

    # Ensure we only keep columns that actually exist
    cols_to_keep <- cols_to_keep[cols_to_keep %in% available_cols]

    # If no columns to keep, return empty table with message
    if (length(cols_to_keep) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay columnas disponibles para mostrar"),
        rownames = FALSE,
        options = list(dom = 't', language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json'))
      ))
    }

    # Select only existing columns
    display_data <- display_data[, cols_to_keep, drop = FALSE]

    # Translate column names using sib_merge_ind_label
    # Only translate names that actually exist in the data
    original_names <- names(display_data)
    tryCatch({
      translated_names <- sib_merge_ind_label(original_names, con = con)
      # Ensure we have the same length and valid names
      if (length(translated_names) == length(original_names) &&
          !any(is.na(translated_names)) &&
          !any(translated_names == "")) {
        names(display_data) <- translated_names
      } else {
        # If translation returns invalid names, keep original names
        if (DEBUG_MODE) message("Warning: Translation returned invalid names, keeping original names")
      }
    }, error = function(e) {
      # If translation fails, keep original names
      if (DEBUG_MODE) message("Warning: Could not translate column names: ", e$message)
    })

    # Rename label to Región (in case it wasn't translated)
    if ("label" %in% names(display_data)) {
      names(display_data)[names(display_data) == "label"] <- "Región"
    }

    # Final validation: ensure all column names are valid and exist
    valid_names <- names(display_data)
    if (length(valid_names) != ncol(display_data)) {
      names(display_data) <- paste0("Col", seq_len(ncol(display_data)))
    }

    DT::datatable(
      display_data,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        searching = TRUE,
        ordering = TRUE,
        stateSave = FALSE,  # Don't save state to avoid column name conflicts
        destroy = TRUE,  # Destroy previous table instance before creating new one
        language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json')
      )
    )
  })

  # Render table data table for modal (reuse main data)
  output$table_data_table <- DT::renderDataTable({
    req(r$main_data)

    # Validate that main_data is a data frame
    if (!is.data.frame(r$main_data) || nrow(r$main_data) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay datos disponibles"),
        rownames = FALSE,
        options = list(dom = 't', language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json'))
      ))
    }

    DT::datatable(
      r$main_data,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        searching = TRUE,
        ordering = TRUE,
        stateSave = FALSE,  # Don't save state to avoid column name conflicts
        destroy = TRUE,  # Destroy previous table instance before creating new one
        language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json')
      )
    )
  })

  # Render cards data table for modal
  output$cards_data_table <- DT::renderDataTable({
    req(r$current_chart_data)

    # Validate that current_chart_data is a data frame
    if (!is.data.frame(r$current_chart_data) || nrow(r$current_chart_data) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay datos disponibles"),
        rownames = FALSE,
        options = list(dom = 't', language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json'))
      ))
    }

    DT::datatable(
      r$current_chart_data,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        searching = TRUE,
        ordering = TRUE,
        stateSave = FALSE,  # Don't save state to avoid column name conflicts
        destroy = TRUE,  # Destroy previous table instance before creating new one
        language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json')
      )
    )
  })

  # Render chart data table for modal
  output$chart_data_table <- DT::renderDataTable({
    req(r$main_data)

    # Validate that main_data is a data frame
    if (!is.data.frame(r$main_data) || nrow(r$main_data) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay datos disponibles"),
        rownames = FALSE,
        options = list(dom = 't', language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json'))
      ))
    }

    display_data <- r$main_data

    # Format indicator column to human-friendly labels (only if it exists)
    if ("indicador" %in% names(display_data)) {
      tryCatch({
        display_data$indicador <- as.character(
          sib_merge_ind_label(as.character(display_data$indicador), con = con)
        )
      }, error = function(e) {
        # If translation fails, keep original values
        if (DEBUG_MODE) message("Warning: Could not translate indicador column: ", e$message)
      })
    }

    # Translate column names (with error handling)
    tryCatch({
      names(display_data) <- sib_merge_ind_label(names(display_data), con = con)
    }, error = function(e) {
      # If translation fails, keep original names
      if (DEBUG_MODE) message("Warning: Could not translate column names: ", e$message)
    })

    # Force friendly headers for common columns (only if they exist)
    if ("indicator" %in% names(display_data)) {
      names(display_data)[names(display_data) == "indicator"] <- "Indicador"
    }
    if ("count" %in% names(display_data)) {
      names(display_data)[names(display_data) == "count"] <- "Número"
    }

    DT::datatable(
      display_data,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        searching = TRUE,
        ordering = TRUE,
        stateSave = FALSE,  # Don't save state to avoid column name conflicts
        destroy = TRUE,  # Destroy previous table instance before creating new one
        language = list(url = '//cdn.datatables.net/plug-ins/2.3.4/i18n/es-ES.json')
      )
    )
  })

  exp_species_table_server("species", r, con, session,
                           loading_fns = list(show = show_loading, hide = hide_loading),
                           debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Species table module initialized")

  exp_debug_server("debug", r, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Debug module initialized")

  exp_debug2_server("debug2", r, debug = DEBUG_MODE)
  if (DEBUG_MODE) message("✓ Debug2 module initialized")

  # Hide loading spinner after all modules are initialized
  observe({
    # Wait for initial data to be ready
    req(r$inputs_ready)
    req(r$sel_region)

    # Small delay to ensure everything is rendered
    shinyjs::delay(500, {
      hide_loading()
      if (DEBUG_MODE) message("✓ App fully loaded, hiding spinner")
    })
  })

  # Close database connection when session ends
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(conmap)
  })
}

shinyApp(ui, server)
