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
#' @export
exp_species_table_server <- function(id, r, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    message("🔧 SPECIES TABLE MODULE INITIALIZED")

    # Observer to track region changes
    observe({
      message("🔄 REGION OBSERVER TRIGGERED")
      message("Current r$sel_region: ", r$sel_region)
    })

    # Fetch species data reactively
    data_especies <- reactive({
      message("🔍 SPECIES DATA REACTIVE TRIGGERED")
      message("r$sel_region: ", r$sel_region)
      message("r$sel_grupo: ", r$sel_grupo)
      message("r$sel_tematica: ", r$sel_tematica)
      
      req(r$sel_region)
      message("✓ r$sel_region requirement met")
      
      # Build parameters for list_species
      params <- list(
        region = r$sel_region,
        grupo = r$sel_grupo,
        tematica = r$sel_tematica
      )
      
      message("=== Species query parameters ===")
      message("Region: ", params$region)
      message("Grupo: ", params$grupo)
      message("Tematica: ", params$tematica)
      
      # Call list_species with current parameters
      tryCatch({
        l_s <- list_species(
          region = params$region,
          grupo = params$grupo,
          tematica = params$tematica,
          con = con
        ) |>
          dplyr::collect()
        
        message("Species query returned ", nrow(l_s), " rows")
        
        # Format for display
        if (nrow(l_s) > 0) {
          formatted_data <- format_species_data(l_s)
          message("✓ Species data formatted successfully")
          formatted_data
        } else {
          message("⚠ No species data returned")
          NULL
        }
      }, error = function(e) {
        message("❌ Error fetching species data: ", e$message)
        message("Error details: ", conditionMessage(e))
        NULL
      })
    })
    
    # Store species data in reactive values
    observe({
      message("🔄 UPDATING r$species_data")
      species_data <- data_especies()
      message("Species data rows: ", if(is.null(species_data)) "NULL" else nrow(species_data))
      r$species_data <- species_data
      message("✓ r$species_data updated")
    })

    # Force summary text to update when species data changes
    observe({
      req(r$species_data)
      message("🔄 FORCING SUMMARY TEXT UPDATE")
      
      # Force the summary to re-render
      output$species_summary <- renderText({
        message("📝 SPECIES SUMMARY RENDERED")
        
        total <- nrow(r$species_data)
        region <- r$sel_region %||% "todas las regiones"
        region <- tools::toTitleCase(gsub("-", " ", region))
        
        tematica_text <- if (is.null(r$sel_tematica)) {
          "todas las temáticas"
        } else {
          tools::toTitleCase(gsub("_", " ", r$sel_tematica))
        }
        
        grupo_text <- ""
        if (!is.null(r$sel_grupo)) {
          grupo <- tools::toTitleCase(gsub("-", " ", r$sel_grupo))
          grupo_text <- paste("del grupo", grupo)
        }
        
        result <- sprintf("Mostrando %s especies para %s en %s %s",
                          format(total, big.mark = ","),
                          tematica_text,
                          region,
                          grupo_text)
        message("Summary text: ", result)
        result
      })
    })

    # Independent species table renderer
    observe({
      req(r$species_data)
      message("🎯 INDEPENDENT SPECIES TABLE RENDERER TRIGGERED")
      
      # Force the table to re-render
      output$list_species <- renderDataTable({
        message("🎨 RENDERING SPECIES TABLE (INDEPENDENT)")
        
        species_data <- r$species_data
        message("Species data rows for table: ", nrow(species_data))
        message("Species data columns: ", paste(names(species_data), collapse = ", "))

        # Check if we have data to display
        if (nrow(species_data) == 0) {
          message("⚠ No data to display in table")
          empty_df <- data.frame(
            "No hay especies" = "No se encontraron especies para los filtros seleccionados"
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
        
        message("✓ Creating DataTable with ", nrow(species_data), " rows")

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
        
        message("✓ DataTable created successfully")
        species_table
      })
    })

    # Show species modal
    observeEvent(input$expand_species, {
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
              sprintf("Mostrando %s especies para %s en %s",
                     format(nrow(r$species_data), big.mark = ","),
                     if (is.null(r$sel_tematica)) "todas las temáticas" else tools::toTitleCase(gsub("_", " ", r$sel_tematica)),
                     tools::toTitleCase(gsub("-", " ", r$sel_region %||% "Colombia"))
              )
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
                       file_prefix = "especies")
  })
} 