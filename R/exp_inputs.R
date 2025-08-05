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
    radioButtons(ns("sel_grupo_type"), "Tipo de grupo",
                 c("Biológico" = "biologico", "Interés de Conservación" = "interes")),
    uiOutput(ns("sel_grupo_opts")),
    hr(),
    uiOutput(ns("sel_tematica_")),
    tags$style(HTML("
      /* Style radio buttons to match tematica module green theme */
      .radio input[type='radio'] {
        appearance: none;
        -webkit-appearance: none;
        -moz-appearance: none;
        width: 14px;
        height: 14px;
        border: 1px solid #ccc;
        border-radius: 50%;
        outline: none;
        cursor: pointer;
        position: relative;
        margin: 0;
        padding: 0;
        vertical-align: middle;
        top: -1px;
      }
      
      .radio input[type='radio']:checked {
        background-color: #006400 !important;
        border-color: #006400 !important;
      }
      
      .radio input[type='radio']:checked::after {
        content: '';
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 4px;
        height: 4px;
        background-color: white;
        border-radius: 50%;
      }
      
      .radio input[type='radio']:hover {
        border-color: #006400;
      }
      
      .radio input[type='radio']:checked:hover {
        background-color: #004d00 !important;
        border-color: #004d00 !important;
      }
      
      .radio label {
        cursor: pointer;
        font-weight: normal;
        color: #333;
        margin-left: 4px;
        vertical-align: middle;
      }
      
      .radio label:hover {
        color: #006400;
      }
      
      /* Style selectize inputs to match green theme */
      .selectize-input {
        border-color: #ccc !important;
        transition: border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
      }
      
      .selectize-input:hover {
        border-color: #006400 !important;
      }
      
      .selectize-input:focus {
        border-color: #006400 !important;
        box-shadow: 0 0 0 0.2rem rgba(0, 100, 0, 0.25) !important;
      }
      
      /* Override all selectize dropdown styling */
      .selectize-dropdown {
        border-color: #006400 !important;
      }
      
      .selectize-dropdown .active {
        background-color: #006400 !important;
        color: white !important;
      }
      
      .selectize-dropdown .active:hover {
        background-color: #004d00 !important;
        color: white !important;
      }
      
      .selectize-dropdown .option {
        color: #333 !important;
      }
      
      .selectize-dropdown .option:hover {
        background-color: #e8f5e8 !important;
        color: #333 !important;
      }
      
      .selectize-dropdown .option.active {
        background-color: #006400 !important;
        color: white !important;
      }
      
      .selectize-dropdown .option.active:hover {
        background-color: #004d00 !important;
        color: white !important;
      }
      
      /* Override any blue styling */
      .selectize-dropdown .option[data-selectable] {
        color: #333 !important;
      }
      
      .selectize-dropdown .option[data-selectable]:hover {
        background-color: #e8f5e8 !important;
        color: #333 !important;
      }
      
      .selectize-dropdown .option[data-selectable].active {
        background-color: #006400 !important;
        color: white !important;
      }
      
      .selectize-dropdown .option[data-selectable].active:hover {
        background-color: #004d00 !important;
        color: white !important;
      }
      
      /* Target the selected state specifically */
      .selectize-dropdown .option.selected {
        background-color: #006400 !important;
        color: white !important;
      }
      
      .selectize-dropdown .option.selected:hover {
        background-color: #004d00 !important;
        color: white !important;
      }
      
      /* Override any Bootstrap or default styling */
      .selectize-dropdown .option.selected[data-selectable] {
        background-color: #006400 !important;
        color: white !important;
      }
      
      .selectize-dropdown .option.selected[data-selectable]:hover {
        background-color: #004d00 !important;
        color: white !important;
      }
      
      /* Force override with higher specificity */
      .selectize-dropdown .option.selected[data-selectable][role='option'] {
        background-color: #006400 !important;
        color: white !important;
      }
      
      .selectize-dropdown .option.selected[data-selectable][role='option']:hover {
        background-color: #004d00 !important;
        color: white !important;
      }
      
      /* Style select elements (fallback) */
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
      req(app_options$region)
      default_select <- NULL
      
      if (!is.null(url_par()$region)) {
        default_select <- tolower(url_par()$region)
        if (debug) {
          message("🌐 URL region parameter: '", default_select, "'")
          message("📋 Available region count: ", length(app_options$region))
        }
        
        # Check if the URL region exists in our options
        if (default_select %in% app_options$region) {
          if (debug) message("✅ URL region found in available regions")
        } else {
          if (debug) message("❌ URL region NOT found in available regions")
          default_select <- app_options$region[1]
        }
        
        if (debug) message("🎯 Final selected value: '", default_select, "'")
      }
      
      selectizeInput(ns("sel_region"), "Seleccione Región",
                     app_options$region,
                     selected = default_select %||% app_options$region[1])
    })

    # Group selector (conditional)
    output$sel_grupo_opts <- renderUI({
      req(input$sel_grupo_type)
      
      default_select <- NULL
      if (!is.null(url_par()$grupo)) {
        default_select <- tolower(url_par()$grupo)
        # Auto-detect group type based on URL parameter
        if (!is.null(default_select) && default_select %in% app_options$grupo_biologico) {
          updateRadioButtons(session, "sel_grupo_type", selected = "biologico")
        } else if (!is.null(default_select) && default_select %in% app_options$grupo_interes) {
          updateRadioButtons(session, "sel_grupo_type", selected = "interes")
        }
      }
      
      if (input$sel_grupo_type == "biologico") {
        selectizeInput(ns("sel_grupo_bio"), "Seleccione grupo biológico",
                       app_options$grupo_biologico,
                       selected = default_select %||% app_options$grupo_biologico[1],
                       options = list(placeholder = "Buscar grupo...", searchField = "text"))
      } else {
        selectizeInput(ns("sel_grupo_int"), "Seleccione grupo de interés",
                       app_options$grupo_interes,
                       selected = default_select %||% app_options$grupo_interes[1],
                       options = list(placeholder = "Buscar grupo...", searchField = "text"))
      }
    })

    # Thematic selector - using the new exp_inputs_tematica module
    # Create the tematica UI
    sel_tematica_ui <- exp_inputs_tematica_ui("tematica")
    
    # Render the tematica UI
    output$sel_tematica_ <- renderUI({
      req(app_options$con)
      sel_tematica_ui
    })
    
    # Get selected group (biology or interest)
    sel_grupo <- reactive({
      req(input$sel_grupo_type)
      if (input$sel_grupo_type == "biologico") {
        return(input$sel_grupo_bio)
      } else {
        return(input$sel_grupo_int)
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
        if (debug) message("✓ r$sel_region initialized to: ", r$sel_region)
      }
      
      # Set grupo type (with fallback)
      if (!is.null(input$sel_grupo_type)) {
        r$sel_grupo_type <- input$sel_grupo_type
        if (debug) message("✓ r$sel_grupo_type initialized to: ", r$sel_grupo_type)
      } else {
        r$sel_grupo_type <- "biologico"
        if (debug) message("✓ r$sel_grupo_type set to default: biologico")
      }
      
      # Set grupo (with fallback)
      if (!is.null(input$sel_grupo_type)) {
        grupo <- sel_grupo()
        if (!is.null(grupo) && grupo != "todos") {
          r$sel_grupo <- grupo
          if (debug) message("✓ r$sel_grupo initialized to: ", r$sel_grupo)
        }
      }
      
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
        if (debug) message("✓ r$sel_region updated to: ", r$sel_region)
      }
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$sel_grupo_type, {
      r$sel_grupo_type <- input$sel_grupo_type
      if (debug) message("✓ r$sel_grupo_type updated to: ", r$sel_grupo_type)
    })
    
    observeEvent(sel_grupo(), {
      req(sel_grupo())
      grupo <- sel_grupo()
      if (!is.null(grupo) && grupo == "todos") grupo <- NULL
      r$sel_grupo <- grupo
      if (debug) message("✓ r$sel_grupo updated to: ", r$sel_grupo)
    })
    

  })
} 