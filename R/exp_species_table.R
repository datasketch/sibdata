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
    # Header with expand button
    div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        h5("Lista de Especies", style = "margin: 0;"),
        actionButton(ns("expand_species"), "Ver lista completa", 
                    class = "btn-sm btn-outline-info",
                    icon = icon("expand"))
    ),
    # Summary text above the table
    div(
      class = "summary-text",
      textOutput(ns("species_summary"))
    ),
    # Species data table
    dataTableOutput(ns("list_species"))
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

    # Fetch species data reactively
    data_especies <- reactive({
      req(r$sel_region)
      
      # Build parameters for list_species
      params <- list(
        region = r$sel_region,
        grupo = r$sel_grupo,
        tematica = r$sel_tematica
      )
      
      # Call list_species with current parameters
      tryCatch({
        message("=== Species query parameters ===")
        message("Region: ", params$region)
        message("Grupo: ", params$grupo)
        message("Tematica: ", params$tematica)
        
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
          format_species_data(l_s)
        } else {
          NULL
        }
      }, error = function(e) {
        message("Error fetching species data: ", e$message)
        message("Error details: ", conditionMessage(e))
        NULL
      })
    })
    
    # Store species data in reactive values
    observe({
      r$species_data <- data_especies()
    })

    # Render summary text based on selected filters and actual data
    output$species_summary <- renderText({
      req(data_especies())
      
      total <- nrow(data_especies())
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
      
      sprintf("Mostrando %s especies para %s en %s %s",
              format(total, big.mark = ","),
              tematica_text,
              region,
              grupo_text)
    })

    # Render species table with custom styling
    output$list_species <- renderDataTable({
      req(data_especies())
      
      species_data <- data_especies()
      
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
        options = get_species_table_options()
      )
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