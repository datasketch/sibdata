
#' Get Thematic Tree Structure
#'
#' Builds a hierarchical tree structure from the tematica database table
#'
#' @param con Database connection
#' @return List containing the thematic tree structure
#' @export
get_tematicas_tree <- function(con) {
  tematica <- sibdata_tematica(con) |>
    filter(activa == "TRUE") |>
    collect()
  tree <- data.tree::FromDataFrameNetwork(tematica)
  l <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "slug",
                                 childrenName = "children")
  l
}

#' Thematic Selection UI Module
#'
#' Creates a hierarchical checkbox interface for thematic selection
#'
#' @param id Module ID
#' @return UI elements for thematic selection
#' @export
exp_inputs_tematica_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
        div(
      class = "tematica-container",
      div(
        class = "tematica-header",
        h5("Temática"),
        actionButton(ns("clear_tematica"), "",
                    icon = icon("refresh"),
                    class = "btn-sm btn-outline-secondary tematica-clear-btn",
                    title = "Limpiar selección")
      ),
      uiOutput(ns("tematica_ui"))
    ),
    tags$style(HTML("
      .tematica-container {
        margin-bottom: 15px;
      }
      .tematica-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 10px;
      }
      .tematica-header h5 {
        margin: 0;
      }
      .tematica-clear-btn {
        padding: 4px 8px;
        font-size: 12px;
      }
      .tematica-options {
        margin-bottom: 10px;
      }

      .tematica-children {
        margin-left: 20px;
        margin-top: -15px;
        margin-bottom: 0px;
        padding-left: 0px;
      }

      .tematica-actions {
        text-align: center;
      }

            /* Style first-level checkboxes to look like radio buttons */
      .tematica-parent input[type='checkbox'] {
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

      .tematica-parent input[type='checkbox']:checked {
        background-color: #006400 !important;
        border-color: #006400 !important;
      }

      .tematica-parent input[type='checkbox']:checked::after {
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

      .tematica-parent input[type='checkbox']:hover {
        border-color: #006400;
      }

      .tematica-parent input[type='checkbox']:checked:hover {
        background-color: #004d00 !important;
        border-color: #004d00 !important;
      }

      .tematica-parent label {
        cursor: pointer;
        font-weight: normal;
        color: #333;
        margin-left: 4px;
        vertical-align: middle;
      }

      .tematica-parent label:hover {
        color: #006400;
      }

      /* Ensure consistent spacing with radio buttons */
      .tematica-parent .checkbox {
        margin-bottom: 0;
        padding: 2px 0;
      }



      /* Style radio buttons to match checkboxes exactly */
      .tematica-children input[type='radio'],
      .tematica-children .radio input[type='radio'] {
        appearance: none !important;
        -webkit-appearance: none !important;
        -moz-appearance: none !important;
        width: 14px !important;
        height: 14px !important;
        border: 1px solid #ccc !important;
        border-radius: 50% !important;
        outline: none !important;
        cursor: pointer !important;
        position: relative !important;
        margin: 0 !important;
        padding: 0 !important;
        vertical-align: middle !important;
        top: -1px !important;
        background-color: transparent !important;
        box-shadow: none !important;
      }

      .tematica-children input[type='radio']:checked,
      .tematica-children .radio input[type='radio']:checked {
        background-color: #006400 !important;
        border-color: #006400 !important;
        box-shadow: none !important;
      }

      .tematica-children input[type='radio']:checked::after,
      .tematica-children .radio input[type='radio']:checked::after {
        content: '' !important;
        position: absolute !important;
        top: 50% !important;
        left: 50% !important;
        transform: translate(-50%, -50%) !important;
        width: 4px !important;
        height: 4px !important;
        background-color: white !important;
        border-radius: 50% !important;
        box-shadow: none !important;
      }

      .tematica-children input[type='radio']:hover,
      .tematica-children .radio input[type='radio']:hover {
        border-color: #006400 !important;
      }

      .tematica-children input[type='radio']:checked:hover,
      .tematica-children .radio input[type='radio']:checked:hover {
        background-color: #004d00 !important;
        border-color: #004d00 !important;
      }

      /* Override any Shiny-specific radio button styling */
      .tematica-children .radio input[type='radio'] {
        background-color: transparent !important;
        border: 1px solid #ccc !important;
        border-radius: 50% !important;
        width: 14px !important;
        height: 14px !important;
        appearance: none !important;
        -webkit-appearance: none !important;
        -moz-appearance: none !important;
        box-shadow: none !important;
      }

      .tematica-children .radio input[type='radio']:checked {
        background-color: #006400 !important;
        border-color: #006400 !important;
        box-shadow: none !important;
      }

      .tematica-children .radio input[type='radio']:checked::after {
        content: '' !important;
        position: absolute !important;
        top: 50% !important;
        left: 50% !important;
        transform: translate(-50%, -50%) !important;
        width: 4px !important;
        height: 4px !important;
        background-color: white !important;
        border-radius: 50% !important;
        box-shadow: none !important;
      }

      /* Force exact same styling as checkboxes */
      .tematica-children input[type='radio'],
      .tematica-children .radio input[type='radio'],
      .tematica-children .form-check input[type='radio'] {
        appearance: none !important;
        -webkit-appearance: none !important;
        -moz-appearance: none !important;
        width: 14px !important;
        height: 14px !important;
        border: 1px solid #ccc !important;
        border-radius: 50% !important;
        outline: none !important;
        cursor: pointer !important;
        position: relative !important;
        margin: 0 !important;
        padding: 0 !important;
        vertical-align: middle !important;
        top: -1px !important;
        background-color: transparent !important;
        box-shadow: none !important;
      }

      .tematica-children input[type='radio']:checked,
      .tematica-children .radio input[type='radio']:checked,
      .tematica-children .form-check input[type='radio']:checked {
        background-color: #006400 !important;
        border-color: #006400 !important;
        box-shadow: none !important;
      }

      .tematica-children input[type='radio']:checked::after,
      .tematica-children .radio input[type='radio']:checked::after,
      .tematica-children .form-check input[type='radio']:checked::after {
        content: '' !important;
        position: absolute !important;
        top: 50% !important;
        left: 50% !important;
        transform: translate(-50%, -50%) !important;
        width: 4px !important;
        height: 4px !important;
        background-color: white !important;
        border-radius: 50% !important;
        box-shadow: none !important;
      }

      .tematica-children label {
        cursor: pointer;
        font-weight: normal;
        color: #333;
        margin-left: 4px;
        vertical-align: middle;
      }

      .tematica-children label:hover {
        color: #006400;
      }

      /* Additional styling for Shiny radio buttons */
      .tematica-children .radio {
        margin-bottom: 0;
        padding: 2px 0;
      }

      .tematica-children .radio label {
        cursor: pointer;
        font-weight: normal;
        color: #333;
        margin-left: 4px;
        vertical-align: middle;
      }

      .tematica-children .radio label:hover {
        color: #006400;
      }

      /* Info icon styling */
      .tematica-info-icon {
        color: #666;
        font-size: 12px;
        margin-left: 4px;
        cursor: help;
        transition: color 0.1s ease;
      }

      .tematica-info-icon:hover {
        color: #006400;
      }

      /* Faster tooltip appearance */
      .tematica-info-icon {
        position: relative;
      }

      .tematica-info-icon:hover::after {
        content: attr(data-tooltip);
        position: absolute;
        top: 50%;
        left: 100%;
        transform: translateY(-50%);
        margin-left: 10px;
        background: #f8f9fa;
        color: #666;
        padding: 10px 15px;
        border-radius: 6px;
        font-size: 13px;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        font-weight: normal;
        text-transform: none;
        letter-spacing: normal;
        white-space: pre-wrap;
        max-width: 450px;
        min-width: 300px;
        z-index: 1000;
        animation: fadeIn 0.1s ease-in;
        border: 1px solid #dee2e6;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
        line-height: 1.5;
        word-wrap: break-word;
      }

      @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }

      /* Parent content layout */
      .tematica-parent-content {
        display: flex;
        align-items: center;
      }

      .tematica-parent-content .checkbox {
        margin-right: 0;
      }

     .tematica-parent{
        margin: 0;
      }

     .tematica-parent .checkbox{
        margin: 0;
      }

      .tematica-parent-content > .form-group {
        margin-bottom: 0px;
      }

      /* Radio option layout */
      .tematica-radio-option {
        display: flex;
        align-items: center;
        margin-bottom: 2px;
      }

      .tematica-radio-content {
        display: flex;
        align-items: center;
        width: 100%;
      }

      .tematica-radio-option label {
        margin-left: 4px;
        margin-right: 4px;
      }

      /* Tooltip icon styling for dynamically added icons */
      .tematica-dynamic-info-icon {
        color: #666;
        font-size: 12px;
        margin-left: 4px;
        cursor: help;
        transition: color 0.1s ease;
      }

      .tematica-dynamic-info-icon:hover {
        color: #006400;
      }

      .tematica-dynamic-info-icon:hover::after {
        content: attr(data-tooltip);
        position: absolute;
        top: 50%;
        left: 100%;
        transform: translateY(-50%);
        margin-left: 10px;
        background: #f8f9fa;
        color: #666;
        padding: 10px 15px;
        border-radius: 6px;
        font-size: 13px;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        font-weight: normal;
        text-transform: none;
        letter-spacing: normal;
        white-space: pre-wrap;
        max-width: 450px;
        min-width: 300px;
        z-index: 1000;
        animation: fadeIn 0.1s ease-in;
        border: 1px solid #dee2e6;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
        line-height: 1.5;
        word-wrap: break-word;
      }
    ")),
    tags$script(HTML("
      // Function to add tooltips to radio buttons
      function addTooltipsToRadioButtons() {
        console.log('Adding tooltips to radio buttons...');

        // Find all tematica children containers
        const childrenContainers = document.querySelectorAll('.tematica-children');
        console.log('Found', childrenContainers.length, 'children containers');

        childrenContainers.forEach((container, index) => {
          console.log('Processing container', index);
          const tooltipsData = container.getAttribute('data-tooltips');
          console.log('Tooltips data:', tooltipsData);

          if (tooltipsData) {
            try {
              const tooltips = JSON.parse(tooltipsData);
              console.log('Parsed tooltips:', tooltips);

              // Find all radio inputs in this container
              const radioInputs = container.querySelectorAll('input[type=\"radio\"]');
              console.log('Found', radioInputs.length, 'radio inputs');

              // Process each radio input and find its corresponding label
              radioInputs.forEach((radioInput, index) => {
                console.log('Processing radio input', index, 'with value:', radioInput.value);
                console.log('Tooltip for this value:', tooltips[radioInput.value]);

                // Skip todas option - it doesn't need a tooltip
                if (radioInput.value === 'todas') {
                  console.log('Skipping todas option');
                  return;
                }

                if (tooltips[radioInput.value]) {
                  // Find the label that contains this radio input
                  const label = radioInput.closest('label');
                  if (label) {
                    console.log('Found label for', radioInput.value, ':', label.textContent.trim());

                    // Check if icon already exists
                    const existingIcon = label.querySelector('.tematica-dynamic-info-icon');
                    if (!existingIcon) {
                      // Create info icon
                      const infoIcon = document.createElement('i');
                      infoIcon.className = 'fas fa-info-circle tematica-dynamic-info-icon';
                      infoIcon.setAttribute('data-tooltip', tooltips[radioInput.value]);
                      infoIcon.style.position = 'relative';

                      // Append icon to label
                      label.appendChild(infoIcon);
                      console.log('Added tooltip icon for:', radioInput.value);
                    } else {
                      console.log('Tooltip icon already exists for:', radioInput.value);
                    }
                  } else {
                    console.log('Could not find label for radio input:', radioInput.value);
                  }
                }
              });
            } catch (e) {
              console.error('Error parsing tooltips data:', e);
            }
          } else {
            console.log('No tooltips data found for container', index);
          }
        });
      }

      // Run when DOM is ready
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', addTooltipsToRadioButtons);
      } else {
        addTooltipsToRadioButtons();
      }

      // Also run when Shiny updates the UI
      if (typeof Shiny !== 'undefined') {
        $(document).on('shiny:value', function(event) {
          console.log('Shiny value event triggered, adding tooltips...');
          setTimeout(addTooltipsToRadioButtons, 100);
        });

        // Also run when children are shown
        $(document).on('shown', '.tematica-children', function() {
          console.log('Tematica children shown, adding tooltips...');
          setTimeout(addTooltipsToRadioButtons, 50);
        });
      }
    "))
  )
}

#' Thematic Selection Server Module
#'
#' Handles the server logic for hierarchical thematic selection
#'
#' @param id Module ID
#' @param tematicas_tree Thematic tree structure from database
#' @param session_main Main session object (optional)
#' @param debug Boolean to control console debug output
#' @return Reactive expression returning the selected tematica slug
#' @export
exp_inputs_tematica_server <- function(id, con, session_main = NULL, debug = FALSE) {
  # cat("🚨 exp_inputs_tematica_server CALLED with id:", id, "debug:", debug, "\n")
  moduleServer(id, function(input, output, session) {
    # cat("🚨 moduleServer FUNCTION CALLED\n")
    ns <- session$ns

    # Safely load tematica tree with error handling
    if (debug) cat("🔄 LOADING tematica tree from database...\n")

    # Load tematica tree - let errors bubble up for debugging
    if (is.null(con) || !DBI::dbIsValid(con)) {
      if (debug) cat("❌ ERROR: Invalid database connection\n")
      stop("Invalid database connection")
    }
    if (debug) cat("✅ Database connection is valid\n")

    tematica_tree <- get_tematicas_tree(con)
    if (debug) cat("✅ get_tematicas_tree() completed\n")

    # cat("🔍 CHECKPOINT 0: Tree loaded, about to continue\n")

    # cat("🔍 CHECKPOINT 1: After tematica_tree loading\n")

    if (debug) {
      cat("=== DEBUG: Tematica module server started ===\n")
      if (is.null(tematica_tree)) {
        cat("❌ ERROR: tematica_tree is NULL\n")
      } else {
        cat("✅ tematica_tree loaded successfully\n")
        if (!is.null(tematica_tree$children)) {
          cat("✅ tematica_tree has", length(tematica_tree$children), "children\n")
        }
      }
      cat("✅ About to create selected_tematica reactive\n")
    }

    # cat("🔍 CHECKPOINT 2: About to create URL reactive\n")

    # URL parameter handling
    url_par <- reactive({
      if (!is.null(session_main)) {
        query <- parseQueryString(session_main$clientData$url_search)
        return(query)
      }
      list()
    })

    # cat("🔍 CHECKPOINT 3: URL reactive created\n")

    # Set initial state from URL parameters - delay to ensure UI is rendered first
    observe({
      req(tematica_tree)

      # Only process URL parameters if session_main is available
      if (is.null(session_main)) return()

      url_params <- url_par()
      if (length(url_params) == 0) return()

      tematica_param <- url_params$tematica
      if (!is.null(tematica_param) && tematica_param != "") {
        # Delay execution to ensure UI inputs are created first
        shinyjs::delay(500, {
        if (debug) cat("Setting tematica from URL:", tematica_param, "\n")

        # Find the tematica in the tree
        l <- tematica_tree
        found_parent <- NULL
        found_child <- NULL

        # First, check if it's a parent slug
        for (x in l$children) {
          if (x$slug == tematica_param) {
            found_parent <- x$slug
            break
          }
          # Check if it's a child slug (first level children)
          if (!is.null(x$children) && length(x$children) > 0) {
            for (child in x$children) {
              if (child$slug == tematica_param) {
                found_parent <- x$slug
                found_child <- child$slug
                break
              }
              # Check if it's a grandchild slug (second level children)
              if (!is.null(child$children) && length(child$children) > 0) {
                for (grandchild in child$children) {
                  if (grandchild$slug == tematica_param) {
                    found_parent <- x$slug
                    found_child <- grandchild$slug
                    break
                  }
                }
                if (!is.null(found_child)) break
              }
            }
            if (!is.null(found_parent)) break
          }
        }

        if (!is.null(found_parent)) {
          # Set the parent checkbox
          updateCheckboxInput(session, found_parent, value = TRUE)
          # Show children
          children_id <- paste0(found_parent, "_children")
          shinyjs::show(children_id)
          # Set child selection if found
          if (!is.null(found_child)) {
            updateRadioButtons(session, children_id, selected = found_child)
          } else {
            # Set default based on category (amenazadas has no "Todas")
            if (identical(found_parent, "amenazadas")) {
              updateRadioButtons(session, children_id, selected = "amenazadas-global")
              shinyjs::show(session$ns("amenazadas_categoria"))
              updateRadioButtons(session, "amenazadas_categoria", selected = "_total")
            } else {
              updateRadioButtons(session, children_id, selected = "todas")
            }
          }
        } else {
          if (debug) cat("Tematica not found in tree:", tematica_param, "\n")
        }
        }) # End shinyjs::delay
      }
    })

    # Create UI inputs using renderUI (works properly with nested modules)
    # cat("🔍 CHECKPOINT 4: About to create renderUI\n")

    output$tematica_ui <- renderUI({
      if (debug) cat("🎨 TEMATICA renderUI CALLED!\n")

      if (is.null(tematica_tree)) {
        if (debug) cat("❌ ERROR: tematica_tree is NULL, cannot create UI\n")
        return(tags$p("Error: No tematica data available"))
      }

      if (debug) cat("✅ tematica_tree is available, proceeding with UI creation\n")

      req(tematica_tree)

      if (debug) cat("✅ req(tematica_tree) passed\n")

      # Get the tree structure
      l <- tematica_tree

      if (debug) cat("✅ Got tree structure, children count:", length(l$children), "\n")

      # Create list to hold all inputs
      all_inputs <- list()

      # Process each first-level item
      purrr::walk(l$children, function(x) {
        # Create first-level checkbox with info icon
        parent_id <- session$ns(x$slug)

        # Create info icon if tooltip or descripcion exists
        info_icon <- ""
        if (!is.null(x$tooltip) || !is.null(x$descripcion)) {
          tooltip_text <- if (!is.null(x$tooltip)) x$tooltip else ""
          desc_text <- if (!is.null(x$descripcion)) x$descripcion else ""
          full_tooltip <- paste(tooltip_text, desc_text, sep = "\n\n")
          full_tooltip <- gsub("\n\n$", "", full_tooltip) # Remove trailing newlines

          info_icon <- tags$i(
            class = "fas fa-info-circle tematica-info-icon",
            `data-tooltip` = full_tooltip
          )
        }

        parent_input <- div(
          class = "tematica-parent",
          div(
            class = "tematica-parent-content",
            checkboxInput(parent_id, x$label, value = FALSE),
            info_icon
          )
        )
        all_inputs <<- c(all_inputs, list(parent_input))

        # Create second-level radio buttons if children exist
        if (!is.null(x$children) && is.list(x$children) && length(x$children) > 0) {

          # Add "Todas" option (except for amenazadas)
          choices <- c()
          if (x$slug != "amenazadas") {
            choices <- c("Todas" = "todas")
            names(choices) <- c("Todas")
          }

          # Add actual children
          child_choices <- purrr::map_chr(x$children, ~.$slug)
          names(child_choices) <- purrr::map_chr(x$children, ~.$label)
          choices <- c(choices, child_choices)

          # Create radio buttons container with info icons
          children_id <- session$ns(paste0(x$slug, "_children"))

          # Create radio buttons using Shiny's radioButtons function for proper input registration
          children_input <- div(
            id = children_id,
            class = "tematica-children",
            style = "display: none;",
            radioButtons(children_id, "", choices = choices, selected = character(0))
          )

          # Special: Amenazadas category selector (EN, CR, VU) under selected child
          if (identical(x$slug, "amenazadas")) {
            amen_cats_id <- session$ns("amenazadas_categoria")
            amen_cat_input <- div(
              id = amen_cats_id,
              class = "tematica-children",
              style = "display: none; margin-left: 36px;",
              radioButtons(amen_cats_id, NULL,
                          choices = c("Total amenazadas" = "_total",
                                      "EN" = "_en",
                                      "CR" = "_cr",
                                      "VU" = "_vu"),
                          selected = character(0))
            )
            all_inputs <<- c(all_inputs, list(children_input, amen_cat_input))
          } else {
            all_inputs <<- c(all_inputs, list(children_input))
          }
          # Store tooltip data for JavaScript to use later
          tooltip_data <- list()
          for (choice_name in names(choices)) {
            choice_value <- choices[choice_name]

            # Find the corresponding child data for tooltip/descripcion
            child_data <- NULL
            if (choice_value != "todas") {
              for (child in x$children) {
                if (child$slug == choice_value) {
                  child_data <- child
                  break
                }
              }
            }

            # Create tooltip data if available
            if (!is.null(child_data) && (!is.null(child_data$tooltip) || !is.null(child_data$descripcion))) {
              tooltip_text <- if (!is.null(child_data$tooltip)) child_data$tooltip else ""
              desc_text <- if (!is.null(child_data$descripcion)) child_data$descripcion else ""
              full_tooltip <- paste(tooltip_text, desc_text, sep = "\n\n")
              full_tooltip <- gsub("\n\n$", "", full_tooltip) # Remove trailing newlines

              tooltip_data[[choice_value]] <- full_tooltip
            }
          }

          # Add tooltip data to the input for JavaScript access
          children_input$attribs[["data-tooltips"]] <- jsonlite::toJSON(tooltip_data, auto_unbox = TRUE)
        }
      })

      if (debug) cat("✅ Finished processing all children, total inputs:", length(all_inputs), "\n")

      # Create the final UI
      final_ui <- do.call(tagList, all_inputs)

      if (debug) cat("✅ TEMATICA UI CREATED SUCCESSFULLY!\n")

      return(final_ui)
    })

    # Handle first-level selection (single selection)
    # Create individual observeEvent for each checkbox to avoid race conditions
    if (!is.null(tematica_tree) && !is.null(tematica_tree$children)) {
      for (x in tematica_tree$children) {
      local({
        current_slug <- x$slug
        observeEvent(input[[current_slug]], {
          req(tematica_tree)
          l <- tematica_tree

          # If this checkbox is checked, uncheck all others
          if (isTRUE(input[[current_slug]])) {
            # Uncheck all other checkboxes
            for (other_x in l$children) {
              if (other_x$slug != current_slug) {
                updateCheckboxInput(session, other_x$slug, value = FALSE)
              }
            }

            # Show children for selected parent
            children_id <- paste0(current_slug, "_children")
            shinyjs::show(children_id)

            # If Amenazadas selected, show category selector and default to _total
            if (identical(current_slug, "amenazadas")) {
              shinyjs::show("amenazadas_categoria")
              if (is.null(input[["amenazadas_categoria"]]) || identical(input[["amenazadas_categoria"]], "")) {
                updateRadioButtons(session, "amenazadas_categoria", selected = "_total")
              }
            }

            # Set default selection based on the category
            current_selection <- input[[children_id]]
            # Ensure a child option is always selected (handle NULL/""/character(0))
            if (length(current_selection) == 0 || identical(current_selection, "")) {
              if (current_slug == "amenazadas") {
                # For amenazadas, select the first option (amenazadas-global)
                updateRadioButtons(session, children_id, selected = "amenazadas-global")
              } else {
                # For other categories, select "Todas"
                updateRadioButtons(session, children_id, selected = "todas")
              }
            }

            # Hide children for all other parents
            for (other_x in l$children) {
              if (other_x$slug != current_slug) {
                other_children_id <- paste0(other_x$slug, "_children")
                shinyjs::hide(other_children_id)
                updateRadioButtons(session, other_children_id, selected = character(0))
              }
            }
          } else {
            # Hide children for this unchecked parent
            children_id <- paste0(current_slug, "_children")
            shinyjs::hide(children_id)
            updateRadioButtons(session, children_id, selected = character(0))

            # Hide Amenazadas category selector when unchecking amenazadas
            if (identical(current_slug, "amenazadas")) {
              shinyjs::hide("amenazadas_categoria")
              updateRadioButtons(session, "amenazadas_categoria", selected = character(0))
            }
          }
        }, ignoreInit = TRUE)
      })
      } # End for loop
    } # End if (!is.null(tematica_tree))

    # Show Amenazadas categories whenever an Amenazadas child is selected
    observeEvent(input[["amenazadas_children"]], {
      sel <- input[["amenazadas_children"]]
      if (!is.null(sel) && sel != "") {
        shinyjs::show("amenazadas_categoria")
        if (is.null(input[["amenazadas_categoria"]]) || identical(input[["amenazadas_categoria"]], "")) {
          updateRadioButtons(session, "amenazadas_categoria", selected = "_total")
        }
      }
    }, ignoreInit = TRUE)

    # Handle clear button
    observeEvent(input$clear_tematica, {
      req(tematica_tree)
      l <- tematica_tree

      # Uncheck all first-level items
      for (x in l$children) {
        updateCheckboxInput(session, x$slug, value = FALSE)
      }

      # Hide all children and clear selections
      for (x in l$children) {
        if (!is.null(x$children) && length(x$children) > 0) {
          children_id <- paste0(x$slug, "_children")
          shinyjs::hide(children_id)
          updateRadioButtons(session, children_id, selected = character(0))
        }
      }

      # Hide and reset amenazadas categoria
      shinyjs::hide("amenazadas_categoria")
      updateRadioButtons(session, "amenazadas_categoria", selected = character(0))
    })



    # Return selected tematica and subtematica (when applicable)
    selected_tematica <- reactive({
      req(tematica_tree)
      l <- tematica_tree

      # Check for first-level selection
      for (i in seq_along(l$children)) {
        x <- l$children[[i]]
        input_value <- input[[x$slug]]

        if (!is.null(input_value) && isTRUE(input_value)) {
          # Check if a child is selected
          if (!is.null(x$children) && length(x$children) > 0) {
            children_id <- paste0(x$slug, "_children")
            child_selection <- input[[children_id]]

            if (!is.null(child_selection) && child_selection != "") {
              # Build structured response
              if (child_selection == "todas") {
                # For "Todas", expose only parent tematica and no subtematica
                return(list(tematica = x$slug, subtematica = NULL))
              }

              # Amenazadas: compose subtematica using category radios
              if (identical(x$slug, "amenazadas")) {
                # Child is either amenazadas-global or amenazadas-nacional
                cat_val <- input[["amenazadas_categoria"]] %||% "_total"
                # Build subtematica only for EN/CR/VU (not for _total)
                sub_slug <- if (identical(cat_val, "_total")) {
                  NULL
                } else {
                  paste0(child_selection, "-", sub("^_", "", cat_val))
                }
                return(list(tematica = child_selection,
                            subtematica = sub_slug,
                            amenazadas_categoria = cat_val))
              }

              # For specific children, only certain parents should expose subtematica
              expose_as_sub <- x$slug %in% c("cites", "exoticas-total")
              if (expose_as_sub) {
                return(list(tematica = x$slug, subtematica = child_selection))
              }

              # Default behavior: return only tematica (child slug) and no subtematica
              return(list(tematica = child_selection, subtematica = NULL))
            } else {
              # If no child is selected in the UI, force-select a default so the selector reflects the state
              if (identical(x$slug, "amenazadas")) {
                updateRadioButtons(session, children_id, selected = "amenazadas-global")
                if (is.null(input[["amenazadas_categoria"]]) || identical(input[["amenazadas_categoria"]], "")) {
                  updateRadioButtons(session, "amenazadas_categoria", selected = "_total")
                }
                return(list(tematica = "amenazadas-global", subtematica = NULL,
                            amenazadas_categoria = input[["amenazadas_categoria"]] %||% "_total"))
              } else {
                updateRadioButtons(session, children_id, selected = "todas")
                return(list(tematica = x$slug, subtematica = NULL))
              }
            }
          }
          # Ensure amenazadas never returns parent-only selection
          if (identical(x$slug, "amenazadas")) {
            updateRadioButtons(session, paste0(x$slug, "_children"), selected = "amenazadas-global")
            if (is.null(input[["amenazadas_categoria"]]) || identical(input[["amenazadas_categoria"]], "")) {
              updateRadioButtons(session, "amenazadas_categoria", selected = "_total")
            }
            return(list(tematica = "amenazadas-global", subtematica = NULL,
                        amenazadas_categoria = input[["amenazadas_categoria"]] %||% "_total"))
          }
          return(list(tematica = x$slug, subtematica = NULL))  # Return parent slug if no child selected
        }
      }

      return(NULL)  # Nothing selected
    })

    # cat("🔍 CHECKPOINT 5: About to return selected_tematica\n")

    # Return the reactive expression
    if (debug) cat("✅ RETURNING selected_tematica reactive\n")
    return(selected_tematica)
  })
}

