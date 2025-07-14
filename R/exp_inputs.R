# exp_inputs.R
# Input Controls Module for SIB Data App (modular version)

exp_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sel_region_")),
    hr(),
    radioButtons(ns("sel_grupo_type"), "Tipo de grupo",
                 c("Biológico" = "biologico", "Interés de Conservación" = "interes")),
    uiOutput(ns("sel_grupo_opts")),
    hr(),
    uiOutput(ns("sel_tematica_"))
  )
}

exp_inputs_server <- function(id, r, app_options, session_main = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
        message("🌐 URL region parameter: '", default_select, "'")
        message("📋 Available region count: ", length(app_options$region))
        
        # Check if the URL region exists in our options
        if (default_select %in% app_options$region) {
          message("✅ URL region found in available regions")
        } else {
          message("❌ URL region NOT found in available regions")
          default_select <- app_options$region[1]
        }
        
        message("🎯 Final selected value: '", default_select, "'")
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
        selectInput(ns("sel_grupo_bio"), "Seleccione grupo biológico",
                    app_options$grupo_biologico,
                    selected = default_select %||% app_options$grupo_biologico[1])
      } else {
        selectInput(ns("sel_grupo_int"), "Seleccione grupo de interés",
                    app_options$grupo_interes,
                    selected = default_select %||% app_options$grupo_interes[1])
      }
    })

    # Thematic selector
    output$sel_tematica_ <- renderUI({
      req(app_options$tematicas)
      default_select <- "todas"
      if (!is.null(url_par()$tematica)) default_select <- tolower(url_par()$tematica)
      radioButtons(ns("sel_tematica"), "Temática", 
                   app_options$tematicas, 
                   selected = default_select)
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

    # Update r reactive values on input changes
    observeEvent(input$sel_region, {
      message("🔧 Region input changed to: ", input$sel_region)
      message("🔧 Input class: ", class(input$sel_region))
      message("🔧 Input is null: ", is.null(input$sel_region))
      message("🔧 Input length: ", length(input$sel_region))
      if (!is.null(input$sel_region)) {
        message("🔧 Input value: '", input$sel_region, "'")
      }
      r$sel_region <- input$sel_region
      message("✓ r$sel_region updated to: ", r$sel_region)
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$sel_grupo_type, {
      r$sel_grupo_type <- input$sel_grupo_type
    })
    
    observeEvent(sel_grupo(), {
      req(sel_grupo())
      grupo <- sel_grupo()
      if (!is.null(grupo) && grupo == "todos") grupo <- NULL
      r$sel_grupo <- grupo
    })
    
    observeEvent(input$sel_tematica, {
      tematica <- input$sel_tematica
      if (!is.null(tematica) && tematica == "todas") tematica <- NULL
      
      # Convert underscores to hyphens for consistency with list_species
      if (!is.null(tematica)) {
        tematica <- gsub("_", "-", tematica)
      }
      
      r$sel_tematica <- tematica
      message("Tematica updated to: ", tematica)
    })
  })
} 