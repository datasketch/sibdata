# exp_inputs.R
# Input Controls Module for SIB Data App (modular version)

#' Input Controls UI Module
#'
#' Creates the left panel input controls for region, group, and theme selection
#'
#' @param id Module ID
#' @return UI elements for input controls
#' @export
exp_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sel_region_")),
    hr(),
    exp_inputs_grupo_ui(ns("grupo")),
    hr(),
    uiOutput(ns("sel_tematica_")),
    tags$style(HTML("
      /* General styling for select elements */
      select {
        border-color: #ccc;
        transition: border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
      }

      select:hover {
        border-color: #006400 !important;
      }

      select:focus {
        border-color: #006400 !important;
        box-shadow: 0 0 0 0.2rem rgba(0, 100, 0, 0.25) !important;
      }
    "))
  )
}

#' Input Controls Server Module
#'
#' Handles the server logic for input controls including region, group, and theme selection
#'
#' @param id Module ID
#' @param r Reactive values object
#' @param app_options Application options from database
#' @param session_main Main session object (optional)
#' @param debug Boolean to control console debug output
#' @export
exp_inputs_server <- function(id, r, app_options, session_main = NULL, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Immediate initialization of default values
    observe({
      if (debug) message("🚀 IMMEDIATE INITIALIZATION OF DEFAULTS")

      # Set default values for reactive values that don't depend on inputs
      if (is.null(r$sel_tipo)) {
        r$sel_tipo <- "registros"
        if (debug) message("✓ r$sel_tipo set to default: registros")
      }

      if (is.null(r$chart_type)) {
        r$chart_type <- "map"
        if (debug) message("✓ r$chart_type set to default: map")
      }

      if (is.null(r$sel_grupo_tipo)) {
        r$sel_grupo_tipo <- "biologico"
        if (debug) message("✓ r$sel_grupo_tipo set to default: biologico")
      }

      if (is.null(r$sel_region_tipo)) {
        r$sel_region_tipo <- "Nacional"
        if (debug) message("✓ r$sel_region_tipo set to default: Nacional")
      }

      if (is.null(r$is_special_region)) {
        r$is_special_region <- FALSE
        if (debug) message("✓ r$is_special_region set to default: FALSE")
      }

      if (is.null(r$has_subtematica)) {
        r$has_subtematica <- FALSE
        if (debug) message("✓ r$has_subtematica set to default: FALSE")
      }

      if (debug) message("✅ Default values initialized")
    })

    # URL parameter handling
    url_par <- reactive({
      if (!is.null(session_main)) {
        query <- parseQueryString(session_main$clientData$url_search)
        return(query)
      }
      list()
    })

    # Region selector
    output$sel_region_ <- renderUI({
      req(app_options$region_grouped)
      default_select <- NULL

      if (!is.null(url_par()$region)) {
        default_select <- tolower(url_par()$region)
        if (debug) {
          message("🌐 URL region parameter: '", default_select, "'")
          message("📋 Available region groups: ", paste(names(app_options$region_grouped), collapse = ", "))
        }

        # Check if the URL region exists in our options
        all_regions <- unlist(app_options$region_grouped)
        if (default_select %in% tolower(all_regions)) {
          if (debug) message("✅ URL region found in available regions")
        } else {
          if (debug) message("❌ URL region NOT found in available regions")
          default_select <- all_regions[1]
        }

        if (debug) message("🎯 Final selected value: '", default_select, "'")
      }

      selectizeInput(ns("sel_region"), "Seleccione Región",
                     choices = app_options$region_grouped,
                     selected = default_select %||% unlist(app_options$region_grouped)[1],
                     options = list(
                       placeholder = "Seleccione una región...",
                       allowClear = TRUE
                     ))
    })

    # Initialize grupo module using proper nested pattern
    selected_grupo <- exp_inputs_grupo_server("grupo", app_options, session_main, debug)

    # Thematic selector - using the new exp_inputs_tematica module
    # Render the tematica UI
    output$sel_tematica_ <- renderUI({
      req(app_options$con)
      exp_inputs_tematica_ui(ns("tematica"))
    })

    # Handle grupo selection from the module
    observe({
      grupo_result <- selected_grupo()
      if (!is.null(grupo_result)) {
        r$sel_grupo_tipo <- grupo_result$type
        r$sel_grupo <- grupo_result$value
        if (debug) {
          message("✓ r$sel_grupo_tipo updated to: ", r$sel_grupo_tipo)
          message("✓ r$sel_grupo updated to: ", r$sel_grupo)
        }
      } else {
        r$sel_grupo <- NULL
        if (debug) message("✓ r$sel_grupo cleared (no selection)")
      }
    })

    # Initialize reactive values when UI is first rendered
    observe({
      # Wait for at least the region input to be available
      req(input$sel_region)

      if (debug) message("🚀 INITIALIZING REACTIVE VALUES FROM INPUTS")

      # Set region
      if (!is.null(input$sel_region) && input$sel_region != "") {
        r$sel_region <- input$sel_region

        # Determine region type based on selection
        region_tipo <- NULL
        if (input$sel_region %in% app_options$region_colombia) {
          region_tipo <- "Nacional"
        } else if (input$sel_region %in% app_options$region_departamentos) {
          region_tipo <- "Departamentos"
        } else if (input$sel_region %in% app_options$region_especial) {
          region_tipo <- "Especial"
        }

        r$sel_region_tipo <- region_tipo

        # Set is_special_region based on specific regions
        special_regions <- c("region-amazonia", "reserva-forestal-la-planada",
                           "resguardo-indigena-pialapi-pueblo-viejo", "bogota-dc")
        r$is_special_region <- input$sel_region %in% special_regions

        if (debug) {
          message("✓ r$sel_region initialized to: ", r$sel_region)
          message("✓ r$sel_region_tipo initialized to: ", r$sel_region_tipo)
          message("✓ r$is_special_region initialized to: ", r$is_special_region)
        }
      }

      # Grupo initialization is now handled by the grupo module observer

      if (debug) message("✅ All reactive values initialized")
    })

    # Initialize tematica module with proper parameters
    # The new module expects: (id, con, session_main, debug)
    if (debug) cat("🔧 ABOUT TO INITIALIZE tematica module\n")
    selected_tematica <- exp_inputs_tematica_server("tematica", app_options$con, session_main, debug)
    if (debug) cat("✅ TEMATICA MODULE INITIALIZED, selected_tematica type:", class(selected_tematica), "\n")

    # Handle tematica selection from the module
    observe({
      if (debug) cat("🔍 TEMATICA OBSERVER TRIGGERED\n")
      if (debug) cat("🔍 selected_tematica function available:", !is.null(selected_tematica), "\n")

      tem_sel <- selected_tematica()
      if (debug) cat("🔍 selected_tematica() returned:", paste(capture.output(str(tem_sel)), collapse = " "), "\n")

      if (!is.null(tem_sel)) {
        # tem_sel is a list with fields tematica and subtematica
        r$sel_tematica <- tem_sel$tematica
        r$sel_subtematica <- tem_sel$subtematica
        if (!is.null(tem_sel$amenazadas_categoria)) {
          r$amenazadas_categoria <- tem_sel$amenazadas_categoria
        }

        # Set has_subtematica based on specific tematicas (parents that have sub-levels)
        subtematica_themes <- c("amenazadas-nacional", "amenazadas-global", "cites", "exoticas-total")
        r$has_subtematica <- r$sel_tematica %in% subtematica_themes

        if (debug) {
          message("✓ r$sel_tematica updated to: ", r$sel_tematica)
          message("✓ r$sel_subtematica updated to: ", if (is.null(r$sel_subtematica)) "NULL" else r$sel_subtematica)
          message("✓ r$amenazadas_categoria updated to: ", if (is.null(r$amenazadas_categoria)) "NULL" else r$amenazadas_categoria)
          message("✓ r$has_subtematica updated to: ", r$has_subtematica)
        }
      } else {
        r$sel_tematica <- NULL
        r$sel_subtematica <- NULL
        r$amenazadas_categoria <- NULL
        r$has_subtematica <- FALSE
        if (debug) {
          message("✓ r$sel_tematica set to NULL (no selection)")
          message("✓ r$sel_subtematica set to NULL (no selection)")
          message("✓ r$amenazadas_categoria set to NULL (no selection)")
          message("✓ r$has_subtematica set to FALSE (no selection)")
        }
      }

      # CRITICAL FIX: Add delay to ensure tematica UI is fully rendered before setting inputs_ready
      # This prevents race conditions between tematica renderUI and map renderLeaflet
      shinyjs::delay(1500, {
        r$inputs_ready <- TRUE
        if (debug) message("✅ Inputs module ready - setting r$inputs_ready = TRUE (with delay)")
        if (debug) cat("🔍 TIMING: inputs_ready set AFTER tematica UI stabilization delay\n")
      })


    })

    # Update r reactive values on input changes
    observeEvent(input$sel_region, {
      if (debug) message("🔧 Region input changed to: ", input$sel_region)
      if (!is.null(input$sel_region) && input$sel_region != "") {
        r$sel_region <- input$sel_region

        # Determine region type based on selection
        region_tipo <- NULL
        if (input$sel_region %in% app_options$region_colombia) {
          region_tipo <- "Nacional"
        } else if (input$sel_region %in% app_options$region_departamentos) {
          region_tipo <- "Departamentos"
        } else if (input$sel_region %in% app_options$region_especial) {
          region_tipo <- "Especial"
        }

        r$sel_region_tipo <- region_tipo

        # Set is_special_region based on specific regions
        special_regions <- c("region-amazonia", "reserva-forestal-la-planada",
                           "resguardo-indigena-pialapi-pueblo-viejo", "bogota-dc")
        r$is_special_region <- input$sel_region %in% special_regions

        if (debug) {
          message("✓ r$sel_region updated to: ", r$sel_region)
          message("✓ r$sel_region_tipo updated to: ", r$sel_region_tipo)
          message("✓ r$is_special_region updated to: ", r$is_special_region)
        }
      }
    }, ignoreNULL = FALSE)


    # Grupo change handling is now managed by the grupo module observer

    # Set inputs ready flag at the very end of module initialization

  })
}
