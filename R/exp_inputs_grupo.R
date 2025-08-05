# exp_inputs_grupo.R
# Grupo Type Selection Module for SIB Data App

#' Grupo Type Selection UI Module
#'
#' Creates a hierarchical checkbox interface for grupo type selection
#'
#' @param id Module ID
#' @return UI elements for grupo type selection
#' @export
exp_inputs_grupo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    div(
      class = "grupo-type-container",
      div(
        class = "grupo-type-header",
        h5("Tipo de grupo")
      ),
      uiOutput(ns("grupo_ui"))
    ),
    tags$style(HTML("
      /* Grupo type styling - similar to tematica */
      .grupo-type-container {
        margin-bottom: 15px;
      }
      
      .grupo-type-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 10px;
      }
      
      .grupo-type-header h5 {
        margin: 0;
      }
      
      .grupo-type-options {
        margin-bottom: 10px;
      }
      
      .grupo-type-parent {
        margin: 0;
      }
      
      .grupo-type-parent .checkbox {
        margin: 0;
      }
      
      .grupo-type-parent-content {
        display: flex;
        align-items: center;
      }
      
      .grupo-type-parent-content .checkbox {
        margin-right: 0;
      }
      
      .grupo-type-parent-content > .form-group {
        margin-bottom: 0px;
      }
      
      .grupo-type-children {
        margin-left: 0px;
        margin-top: -15px;
        margin-bottom: 0px;
        padding-left: 10px;
      }
      
      /* Style grupo type checkboxes to look like radio buttons */
      .grupo-type-parent input[type='checkbox'] {
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
      
      .grupo-type-parent input[type='checkbox']:checked {
        background-color: #006400 !important;
        border-color: #006400 !important;
      }
      
      .grupo-type-parent input[type='checkbox']:checked::after {
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
      
      .grupo-type-parent input[type='checkbox']:hover {
        border-color: #006400;
      }
      
      .grupo-type-parent input[type='checkbox']:checked:hover {
        background-color: #004d00 !important;
        border-color: #004d00 !important;
      }
      
      .grupo-type-parent label {
        cursor: pointer;
        font-weight: normal;
        color: #333;
        margin-left: 4px;
        vertical-align: middle;
      }
      
      .grupo-type-parent label:hover {
        color: #006400;
      }
      
      .grupo-type-parent .checkbox {
        margin-bottom: 0;
        padding: 2px 0;
      }
    "))
  )
}

#' Grupo Type Selection Server Module
#'
#' Handles the server logic for grupo type selection
#'
#' @param id Module ID
#' @param app_options Application options from database
#' @param session_main Main session object (optional)
#' @param debug Boolean to control console debug output
#' @return Reactive expression returning the selected grupo type and value
#' @export
exp_inputs_grupo_server <- function(id, app_options, session_main = NULL, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (debug) {
      cat("=== DEBUG: Grupo module server started ===\n")
      cat("App options available:", !is.null(app_options), "\n")
      if (!is.null(app_options)) {
        cat("Biological groups count:", length(app_options$grupo_biologico), "\n")
        cat("Interest groups count:", length(app_options$grupo_interes), "\n")
      }
      cat("==============================\n")
    }

    # URL parameter handling
    url_par <- reactive({
      if (!is.null(session_main)) {
        query <- parseQueryString(session_main$clientData$url_search)
        return(query)
      }
      list()
    })

    # Set initial state from URL parameters
    observe({
      req(app_options$grupo_biologico)
      req(app_options$grupo_interes)
      req(url_par())

      grupo_param <- url_par()$grupo
      if (!is.null(grupo_param) && grupo_param != "") {
        cat("=== DEBUG: Setting initial grupo from URL ===\n")
        cat("URL parameter grupo:", grupo_param, "\n")

        # Auto-detect group type based on URL parameter
        if (grupo_param %in% app_options$grupo_biologico) {
          cat("Found in biological groups\n")
          # Set biologico checkbox
          tryCatch({
            updateCheckboxInput(session, "grupo_biologico", value = TRUE)
            cat("  ✓ Set biologico checkbox\n")

            # Show biologico children
            children_id <- paste0("grupo_biologico", "_children")
            shinyjs::show(children_id)
            cat("  ✓ Showed biologico children\n")

            # Set biologico value
            tryCatch({
              updateSelectizeInput(session, children_id, selected = grupo_param)
              cat("  ✓ Set biologico value:", grupo_param, "\n")
            }, error = function(e) {
              cat("  ✗ Error setting biologico value:", e$message, "\n")
            })
          }, error = function(e) {
            cat("  ✗ Error setting biologico checkbox:", e$message, "\n")
          })
        } else if (grupo_param %in% app_options$grupo_interes) {
          cat("Found in interest groups\n")
          # Set interes checkbox
          tryCatch({
            updateCheckboxInput(session, "grupo_interes", value = TRUE)
            cat("  ✓ Set interes checkbox\n")

            # Show interes children
            children_id <- paste0("grupo_interes", "_children")
            shinyjs::show(children_id)
            cat("  ✓ Showed interes children\n")

            # Set interes value
            tryCatch({
              updateSelectizeInput(session, children_id, selected = grupo_param)
              cat("  ✓ Set interes value:", grupo_param, "\n")
            }, error = function(e) {
              cat("  ✗ Error setting interes value:", e$message, "\n")
            })
          }, error = function(e) {
            cat("  ✗ Error setting interes checkbox:", e$message, "\n")
          })
        } else {
          cat("  ✗ Grupo not found in any group type:", grupo_param, "\n")
          # Set default biologico selection
          tryCatch({
            updateCheckboxInput(session, "grupo_biologico", value = TRUE)
            cat("  ✓ Set biologico checkbox (default)\n")

            # Show biologico children
            children_id <- paste0("grupo_biologico", "_children")
            shinyjs::show(children_id)
            cat("  ✓ Showed biologico children\n")

            # Set default value for biologico selectize
            tryCatch({
              updateSelectizeInput(session, children_id, selected = "todos")
              cat("  ✓ Set biologico default value: todos\n")
            }, error = function(e) {
              cat("  ✗ Error setting biologico default value:", e$message, "\n")
            })
          }, error = function(e) {
            cat("  ✗ Error setting biologico checkbox:", e$message, "\n")
          })
        }
        cat("=== END DEBUG: URL parameter handling ===\n\n")
      } else {
        # No URL parameter - set default biologico selection
        cat("=== DEBUG: Setting default grupo selection ===\n")
        tryCatch({
          updateCheckboxInput(session, "grupo_biologico", value = TRUE)
          cat("  ✓ Set biologico checkbox (default)\n")

          # Show biologico children
          children_id <- paste0("grupo_biologico", "_children")
          shinyjs::show(children_id)
          cat("  ✓ Showed biologico children\n")

          # Set default value for biologico selectize
          tryCatch({
            updateSelectizeInput(session, children_id, selected = "todos")
            cat("  ✓ Set biologico default value: todos\n")
          }, error = function(e) {
            cat("  ✗ Error setting biologico default value:", e$message, "\n")
          })
        }, error = function(e) {
          cat("  ✗ Error setting biologico checkbox:", e$message, "\n")
        })
        cat("=== END DEBUG: Default selection ===\n\n")
      }
    })

    # Create UI inputs using renderUI with correct namespace
    output$grupo_ui <- renderUI({
      cat("=== DEBUG: Creating grupo UI ===\n")
      cat("Session namespace test:", session$ns("test"), "\n")
      cat("Module ID:", id, "\n")
      cat("Session namespace for 'grupo_biologico':", session$ns("grupo_biologico"), "\n")
      cat("Session namespace for 'grupo_interes':", session$ns("grupo_interes"), "\n")
      cat("Session namespace for 'grupo_biologico_children':", session$ns("grupo_biologico_children"), "\n")
      cat("Session namespace for 'grupo_interes_children':", session$ns("grupo_interes_children"), "\n")
      
      req(app_options$grupo_biologico)
      req(app_options$grupo_interes)

      if (debug) {
        cat("Creating UI inputs\n")
      }

      # Create list to hold all inputs
      all_inputs <- list()

      # Create checkbox for "Biológico"
      biologico_input <- div(
        class = "grupo-type-parent",
        div(
          class = "grupo-type-parent-content",
          checkboxInput(session$ns("grupo_biologico"), "Biológico", value = FALSE)
        )
      )
      all_inputs <- c(all_inputs, list(biologico_input))

      if (debug) {
        cat("  Created biologico checkbox with ID:", session$ns("grupo_biologico"), "\n")
      }

      # Create children container for "Biológico"
      biologico_children_input <- div(
        id = session$ns("grupo_biologico_children"),
        class = "grupo-type-children",
        style = "display: none;",
        selectizeInput(session$ns("grupo_biologico_children"), "",
                       app_options$grupo_biologico,
                       selected = "todos",
                       options = list(placeholder = "Buscar grupo...", searchField = "text"))
      )
      all_inputs <- c(all_inputs, list(biologico_children_input))

      if (debug) {
        cat("  Created biologico children with ID:", session$ns("grupo_biologico_children"), "\n")
      }

      # Create checkbox for "Interés de Conservación"
      interes_input <- div(
        class = "grupo-type-parent",
        div(
          class = "grupo-type-parent-content",
          checkboxInput(session$ns("grupo_interes"), "Interés de Conservación", value = FALSE)
        )
      )
      all_inputs <- c(all_inputs, list(interes_input))

      if (debug) {
        cat("  Created interes checkbox with ID:", session$ns("grupo_interes"), "\n")
      }

      # Create children container for "Interés de Conservación"
      interes_children_input <- div(
        id = session$ns("grupo_interes_children"),
        class = "grupo-type-children",
        style = "display: none;",
        selectizeInput(session$ns("grupo_interes_children"), "",
                       app_options$grupo_interes,
                       selected = "todos",
                       options = list(placeholder = "Buscar grupo...", searchField = "text"))
      )
      all_inputs <- c(all_inputs, list(interes_children_input))

      if (debug) {
        cat("  Created interes children with ID:", session$ns("grupo_interes_children"), "\n")
      }

      if (debug) {
        cat("Total inputs created:", length(all_inputs), "\n")
        cat("=== END DEBUG: UI creation ===\n")
      }

      # Create the final UI
      final_ui <- do.call(tagList, all_inputs)
      
      if (debug) {
        cat("✓ renderUI completed\n")
      }
      
      final_ui
    })

    # Handle first-level selection (single selection)
    observeEvent(input$grupo_biologico, {
      req(app_options$grupo_biologico)
      req(app_options$grupo_interes)

      cat("=== DEBUG: observeEvent triggered for grupo_biologico ===\n")
      cat("Timestamp:", Sys.time(), "\n")
      cat("Value:", input$grupo_biologico, "\n")
      cat("Input exists:", !is.null(input$grupo_biologico), "\n")
      cat("Input ID being observed: grupo_biologico\n")
      cat("Expected full input ID:", session$ns("grupo_biologico"), "\n")

      # If this checkbox is checked, uncheck all others
      if (isTRUE(input$grupo_biologico)) {
        cat("Checkbox grupo_biologico is checked. Unchecking others...\n")

        # Uncheck interes checkbox
        tryCatch({
          updateCheckboxInput(session, "grupo_interes", value = FALSE)
          cat("  ✓ Unchecked: grupo_interes\n")
        }, error = function(e) {
          cat("  ✗ Error unchecking grupo_interes:", e$message, "\n")
        })

        # Show children for selected parent
        children_id <- paste0("grupo_biologico", "_children")
        cat("  ✓ Showing children for: grupo_biologico\n")
        shinyjs::show(children_id)

        # Hide children for other parent
        other_children_id <- paste0("grupo_interes", "_children")
        cat("  ✗ Hiding children for: grupo_interes\n")
        shinyjs::hide(other_children_id)

      } else {
        cat("Checkbox grupo_biologico is unchecked. Hiding its children...\n")

        # Hide children for this unchecked parent
        children_id <- paste0("grupo_biologico", "_children")
        cat("  ✗ Hiding children for: grupo_biologico\n")
        shinyjs::hide(children_id)

        tryCatch({
          updateSelectizeInput(session, children_id, selected = character(0))
          cat("  ✓ Cleared selectize for: grupo_biologico\n")
        }, error = function(e) {
          cat("  ✗ Error clearing selectize for grupo_biologico:", e$message, "\n")
        })
      }

      cat("=== END DEBUG: observeEvent for grupo_biologico ===\n\n")
    }, ignoreInit = TRUE)

    observeEvent(input$grupo_interes, {
      req(app_options$grupo_biologico)
      req(app_options$grupo_interes)

      cat("=== DEBUG: observeEvent triggered for grupo_interes ===\n")
      cat("Timestamp:", Sys.time(), "\n")
      cat("Value:", input$grupo_interes, "\n")
      cat("Input exists:", !is.null(input$grupo_interes), "\n")
      cat("Input ID being observed: grupo_interes\n")
      cat("Expected full input ID:", session$ns("grupo_interes"), "\n")

      # If this checkbox is checked, uncheck all others
      if (isTRUE(input$grupo_interes)) {
        cat("Checkbox grupo_interes is checked. Unchecking others...\n")

        # Uncheck biologico checkbox
        tryCatch({
          updateCheckboxInput(session, "grupo_biologico", value = FALSE)
          cat("  ✓ Unchecked: grupo_biologico\n")
        }, error = function(e) {
          cat("  ✗ Error unchecking grupo_biologico:", e$message, "\n")
        })

        # Show children for selected parent
        children_id <- paste0("grupo_interes", "_children")
        cat("  ✓ Showing children for: grupo_interes\n")
        shinyjs::show(children_id)

        # Hide children for other parent
        other_children_id <- paste0("grupo_biologico", "_children")
        cat("  ✗ Hiding children for: grupo_biologico\n")
        shinyjs::hide(other_children_id)

      } else {
        cat("Checkbox grupo_interes is unchecked. Hiding its children...\n")

        # Hide children for this unchecked parent
        children_id <- paste0("grupo_interes", "_children")
        cat("  ✗ Hiding children for: grupo_interes\n")
        shinyjs::hide(children_id)

        tryCatch({
          updateSelectizeInput(session, children_id, selected = character(0))
          cat("  ✓ Cleared selectize for: grupo_interes\n")
        }, error = function(e) {
          cat("  ✗ Error clearing selectize for grupo_interes:", e$message, "\n")
        })
      }

      cat("=== END DEBUG: observeEvent for grupo_interes ===\n\n")
    }, ignoreInit = TRUE)

    # Return selected grupo type and value
    selected_grupo <- reactive({
      if (debug) {
        cat("=== DEBUG: selected_grupo reactive triggered ===\n")
      }

      # Check for biologico selection
      biologico_selected <- input$grupo_biologico
      biologico_value <- input$grupo_biologico_children

      if (debug) {
        cat("Biologico selected:", biologico_selected, "\n")
        cat("Biologico value:", biologico_value, "\n")
      }

      if (!is.null(biologico_selected) && isTRUE(biologico_selected)) {
        if (!is.null(biologico_value) && biologico_value != "") {
          if (debug) cat("Returning biologico value:", biologico_value, "\n")
          return(list(type = "biologico", value = biologico_value))
        }
      }

      # Check for interes selection
      interes_selected <- input$grupo_interes
      interes_value <- input$grupo_interes_children

      if (debug) {
        cat("Interes selected:", interes_selected, "\n")
        cat("Interes value:", interes_value, "\n")
      }

      if (!is.null(interes_selected) && isTRUE(interes_selected)) {
        if (!is.null(interes_value) && interes_value != "") {
          if (debug) cat("Returning interes value:", interes_value, "\n")
          return(list(type = "interes", value = interes_value))
        }
      }

      if (debug) cat("No selection found, returning NULL\n")
      return(NULL)  # Nothing selected
    })

    # Return the reactive expression
    selected_grupo
  })
} 