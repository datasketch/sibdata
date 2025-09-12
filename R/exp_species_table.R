# exp_species_table.R
# Species Table Module for SIB Data App (modular version)

#' Species Table UI Module
#'
#' Creates the right panel species table with summary and download functionality
#'
#' @param id Module ID
#' @return UI elements for species table
#' @export
exp_species_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Header with title and expand button
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
      h5(style = "margin: 0;", "Lista de Especies"),
      actionButton(
        ns("expand_species"),
        "Ver lista completa",
        class = "btn-sm btn-outline-info",
        icon = icon("expand")
      )
    ),
    
    # Summary text
    div(
      class = "summary-text",
      textOutput(ns("species_summary"))
    ),
    
    # Table container
    div(
      class = "species-table-container",
      dataTableOutput(ns("list_species"))
    )
  )
}

#' Species Table Server Module
#'
#' Handles the server logic for species table including data fetching and modal display
#'
#' @param id Module ID
#' @param r Reactive values object
#' @param con Database connection
#' @param session Shiny session object for URL parameter handling
#' @param debug Boolean to control console debug output
#' @export
exp_species_table_server <- function(id, r, con, session = NULL, loading_fns = NULL, debug = FALSE) {
  moduleServer(id, function(input, output, session_module) {
    ns <- session_module$ns
    
    if (debug) message("🔧 SPECIES TABLE MODULE INITIALIZED")

    # Track if lista_especies modal has been automatically shown from URL parameter in this session
    auto_modal_shown <- reactiveVal(FALSE)
    
    if (debug) message("✓ Auto modal tracking initialized - auto_modal_shown = FALSE")

    # URL parameter handling for lista_especies
    url_par <- reactive({
      if (!is.null(session)) {
        query <- parseQueryString(session$clientData$url_search)
        return(query)
      }
      list()
    })

    # Observer to track region changes
    observe({
      if (debug) {
        message("🔄 REGION OBSERVER TRIGGERED")
        message("Current r$sel_region: ", r$sel_region)
      }
    })

    # Fetch species data reactively
    data_especies <- reactive({
      if (debug) {
        message("🔍 SPECIES DATA REACTIVE TRIGGERED")
        message("r$sel_region: ", r$sel_region)
        message("r$sel_grupo: ", r$sel_grupo)
        message("r$sel_tematica: ", r$sel_tematica)
      }
      
      req(r$sel_region)
      if (debug) message("✓ r$sel_region requirement met")
      
      # Show loading for species data (can take time)
      if (!is.null(loading_fns)) {
        loading_fns$show("Cargando lista de especies...")
      }
      
      # Build parameters for list_species
      # Prefer subtemática when present; else use temática (hyphens/underscores handled downstream)
      tem_param <- NULL
      if (!is.null(r$sel_subtematica) && nzchar(r$sel_subtematica)) {
        tem_param <- r$sel_subtematica
      } else if (!is.null(r$sel_tematica) && nzchar(r$sel_tematica)) {
        tem_param <- r$sel_tematica
      } else if (!is.null(r$tematica) && nzchar(r$tematica)) {
        tem_param <- r$tematica
      }

      params <- list(
        region = r$sel_region,
        grupo = r$sel_grupo,
        tematica = tem_param
      )
      
      if (debug) {
        message("=== Species query parameters ===")
        message("Region: ", params$region)
        message("Grupo: ", params$grupo)
        message("Tematica (effective): ", if (is.null(params$tematica)) "NULL" else params$tematica)
      }
      
      # Call list_species with current parameters
      tryCatch({
        l_s <- list_species(
          region = params$region,
          grupo = params$grupo,
          tematica = params$tematica,
          con = con
        ) |>
          dplyr::collect()
        
        if (debug) message("Species query returned ", nrow(l_s), " rows")
        
        # Format for display
        result <- if (nrow(l_s) > 0) {
          formatted_data <- format_species_data(l_s)
          if (debug) message("✓ Species data formatted successfully")
          formatted_data
        } else {
          if (debug) message("⚠ No species data returned")
          NULL
        }
        
        # Hide loading after species data is processed
        if (!is.null(loading_fns)) {
          shinyjs::delay(100, loading_fns$hide())
        }
        
        result
        
      }, error = function(e) {
        if (debug) {
          message("❌ Error fetching species data: ", e$message)
          message("Error details: ", conditionMessage(e))
        }
        
        # Hide loading on error
        if (!is.null(loading_fns)) {
          loading_fns$hide()
        }
        
        NULL
      })
    })
    
    # Store species data in reactive values
    observe({
      if (debug) message("🔄 UPDATING r$species_data")
      species_data <- data_especies()
      if (debug) message("Species data rows: ", if(is.null(species_data)) "NULL" else nrow(species_data))
      r$species_data <- species_data
      if (debug) message("✓ r$species_data updated")
    })

    # Summary text renderer
    output$species_summary <- renderText({
      if (debug) message("📝 SPECIES SUMMARY RENDERED")
      
      # Always use the current species list row count
      total <- if (is.null(r$species_data)) 0 else nrow(r$species_data)
      
      # Region label
      region <- r$sel_region %||% "todas las regiones"
      region <- tools::toTitleCase(gsub("-", " ", region))
      
      # Temática label: prefer subtemática; handle CITES casing and roman numerals
      tem_slug <- if (!is.null(r$sel_subtematica) && nzchar(r$sel_subtematica)) {
        r$sel_subtematica
      } else if (!is.null(r$sel_tematica) && nzchar(r$sel_tematica)) {
        r$sel_tematica
      } else {
        NULL
      }
      tematica_text <- if (is.null(tem_slug)) {
        "todas las temáticas"
      } else if (grepl("^cites", tem_slug)) {
        suf <- sub("^cites[-_]", "", tem_slug)
        roman <- toupper(gsub("-", " ", suf))
        paste("CITES", roman)
      } else {
        parts <- unlist(strsplit(tem_slug, "[-_]"))
        paste(tools::toTitleCase(parts), collapse = "-")
      }
      
      # Grupo label: show 'Todos' when no group selected
      grupo_val <- r$sel_grupo
      grupo_label <- if (is.null(grupo_val) || grupo_val == "" || tolower(grupo_val) == "todos") {
        "Todos"
      } else {
        tools::toTitleCase(gsub("-", " ", grupo_val))
      }
      grupo_text <- paste("del grupo", grupo_label)
      
      result <- sprintf("Mostrando %s especies para %s en %s %s",
                        format(total, big.mark = ",", scientific = FALSE),
                        tematica_text,
                        region,
                        grupo_text)
      if (debug) message("Summary text: ", result)
      result
    })

    # Species table renderer
    output$list_species <- renderDataTable({
      if (debug) message("🎨 RENDERING SPECIES TABLE")
      
      species_data <- r$species_data
      if (debug) {
        message("Species data rows for table: ", if(is.null(species_data)) "NULL" else nrow(species_data))
        message("Species data columns: ", if(is.null(species_data)) "NULL" else paste(names(species_data), collapse = ", "))
      }

      # Check if we have data to display
      if (is.null(species_data) || nrow(species_data) == 0) {
        if (debug) message("⚠ No data to display in table")
        empty_df <- data.frame(
          "No hay especies" = "No se encontraron especies para los filtros seleccionados",
          check.names = FALSE
        )
        return(DT::datatable(
          empty_df,
          rownames = FALSE,
          options = list(
            dom = 't',
            searching = FALSE,
            info = FALSE,
            paging = FALSE
          )
        ))
      }
      
      # Format links for GBIF and CBC columns
      if ("GBIF" %in% names(species_data)) {
        species_data$GBIF <- ifelse(
          is.na(species_data$GBIF) | species_data$GBIF == "", 
          "",
          paste0("<a href='", species_data$GBIF, "' target='_blank'>GBIF</a>")
        )
      }
      
      if ("CBC" %in% names(species_data)) {
        species_data$CBC <- ifelse(
          is.na(species_data$CBC) | species_data$CBC == "", 
          "",
          paste0("<a href='", species_data$CBC, "' target='_blank'>CBC</a>")
        )
      }
      
      if (debug) message("✓ Creating DataTable with ", nrow(species_data), " rows")

      # Create the full species table with proper options
      species_table <- DT::datatable(
        species_data,
        rownames = FALSE,
        selection = 'none',
        escape = FALSE,
        options = c(
          get_species_table_options(),
          list(
            scrollY = "300px",
            scrollCollapse = TRUE
          )
        )
      )
      
      if (debug) message("✓ DataTable created successfully")
      species_table
    })

    # Function to show species modal
    show_species_modal <- function() {
      req(r$species_data)
      
      showModal(modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span("Lista Completa de Especies"),
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
          div(class = "summary-text", style = "margin-bottom: 15px;",
              {
                total <- if (is.null(r$species_data)) 0 else nrow(r$species_data)
                region <- tools::toTitleCase(gsub("-", " ", r$sel_region %||% "Colombia"))
                tem_slug <- if (!is.null(r$sel_subtematica) && nzchar(r$sel_subtematica)) r$sel_subtematica else r$sel_tematica
                tem_txt <- if (is.null(tem_slug)) {
                  "todas las temáticas"
                } else if (grepl("^cites", tem_slug)) {
                  suf <- sub("^cites[-_]", "", tem_slug)
                  roman <- toupper(gsub("-", " ", suf))
                  paste("CITES", roman)
                } else {
                  parts <- unlist(strsplit(tem_slug, "[-_]"))
                  paste(tools::toTitleCase(parts), collapse = "-")
                }
                grupo_val <- r$sel_grupo
                grupo_label <- if (is.null(grupo_val) || grupo_val == "" || tolower(grupo_val) == "todos") {
                  "Todos"
                } else {
                  tools::toTitleCase(gsub("-", " ", grupo_val))
                }
                sprintf("Mostrando %s especies para %s en %s del grupo %s",
                        format(total, big.mark = ",", scientific = FALSE),
                        tem_txt,
                        region,
                        grupo_label)
              }
          ),
          div(style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
              downloadTableUI(ns("species_modal_download"), 
                             dropdownLabel = "Descargar especies", 
                             formats = c("csv", "xlsx", "json"), 
                             display = "dropdown",
                             dropdownWidth = 200)
          ),
          DT::dataTableOutput(ns("species_modal_table"))
        ),
        footer = NULL,
        easyClose = TRUE
      ))
    }

    # Show species modal on button click
    observeEvent(input$expand_species, {
      show_species_modal()
    })

    # Auto-show species modal based on URL parameter (only once per session)
    observe({
      req(r$species_data)
      
      # Check if lista_especies parameter is present and true
      if (!is.null(url_par()$lista_especies) && 
          tolower(url_par()$lista_especies) == "true") {
        
        if (auto_modal_shown()) {
          if (debug) message("⚠ Auto modal already shown in this session - skipping")
        } else {
          if (debug) message("🌐 URL parameter lista_especies=true detected - opening modal (first time)")
          
          # Mark auto modal as shown for this session
          auto_modal_shown(TRUE)
          if (debug) message("✓ Auto modal marked as shown for this session")
          
          # Use a small delay to ensure the modal renders properly
          shinyjs::delay(500, {
            show_species_modal()
          })
        }
      }
    })
    
    # Render species table in modal
    output$species_modal_table <- DT::renderDataTable({
      req(r$species_data)
      
      species_data <- r$species_data
      
      # Format links for GBIF and CBC columns
      if ("GBIF" %in% names(species_data)) {
        species_data$GBIF <- ifelse(
          is.na(species_data$GBIF) | species_data$GBIF == "", 
          "",
          paste0("<a href='", species_data$GBIF, "' target='_blank'>GBIF</a>")
        )
      }
      
      if ("CBC" %in% names(species_data)) {
        species_data$CBC <- ifelse(
          is.na(species_data$CBC) | species_data$CBC == "", 
          "",
          paste0("<a href='", species_data$CBC, "' target='_blank'>CBC</a>")
        )
      }
      
      DT::datatable(
        species_data,
        rownames = FALSE,
        selection = 'none',
        escape = FALSE,
        options = list(
          dom = 'Bftsp',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          scrollX = TRUE,
          scrollY = "400px",
          pageLength = 25,
          searching = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#4ad3ac', 'color': '#ffffff'});",
            "}"
          )
        )
      )
    })
    
    # Download species table server for modal
    downloadTableServer("species_modal_download", 
                       element = reactive(r$species_data), 
                       formats = c("csv", "xlsx", "json"),
                       file_prefix = "especies",
                       debug = debug)
  })
} 