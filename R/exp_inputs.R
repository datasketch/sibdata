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
      
      if (is.null(r$sel_grupo_type)) {
        r$sel_grupo_type <- "biologico"
        if (debug) message("✓ r$sel_grupo_type set to default: biologico")
      }
      
      if (is.null(r$sel_region_tipo)) {
        r$sel_region_tipo <- "Nacional"
        if (debug) message("✓ r$sel_region_tipo set to default: Nacional")
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
        r$sel_grupo_type <- grupo_result$type
        r$sel_grupo <- grupo_result$value
        if (debug) {
          message("✓ r$sel_grupo_type updated to: ", r$sel_grupo_type)
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
        
        if (debug) {
          message("✓ r$sel_region initialized to: ", r$sel_region)
          message("✓ r$sel_region_tipo initialized to: ", r$sel_region_tipo)
        }
      }
      
      # Grupo initialization is now handled by the grupo module observer
      
      if (debug) message("✅ All reactive values initialized")
    })

    # Initialize tematica module with proper parameters
    # The new module expects: (id, con, session_main, debug)
    selected_tematica <- exp_inputs_tematica_server("tematica", app_options$con, session_main, debug)
    
    # Handle tematica selection from the module
    observe({
      tematica <- selected_tematica()
      if (!is.null(tematica)) {
        r$sel_tematica <- tematica
        if (debug) message("✓ r$sel_tematica updated to: ", r$sel_tematica)
      } else {
        r$sel_tematica <- NULL
        if (debug) message("✓ r$sel_tematica set to NULL (no selection)")
      }
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
        
        if (debug) {
          message("✓ r$sel_region updated to: ", r$sel_region)
          message("✓ r$sel_region_tipo updated to: ", r$sel_region_tipo)
        }
      }
    }, ignoreNULL = FALSE)
    
    
    # Grupo change handling is now managed by the grupo module observer
    

  })
} 