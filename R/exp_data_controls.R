# exp_data_controls.R
# Dynamic data controls module for SIB Data App (subcategories, total/estimadas)

#' Data Controls UI Module
#' 
#' Creates dynamic UI controls for subcategories and data options
#' Based on original app logic (lines 432-465)
#'
#' @param id Module ID
#' @return UI elements for data controls
#' @export
exp_data_controls_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Dynamic controls container
    div(
      style = "margin-bottom: 10px;",
      uiOutput(ns("dynamic_controls"))
    )
  )
}

#' Data Controls Server Module
#' 
#' Handles dynamic data controls logic including subcategories
#' Based on original app logic (lines 432-465, 383-395)
#'
#' @param id Module ID
#' @param r Reactive values object
#' @return Server logic for data controls
#' @export
exp_data_controls_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper functions for determining control visibility
    is_amenazadas_or_cites_or_exoticas <- reactive({
      tematica <- r$sel_tematica
      if(is.null(tematica)) return(FALSE)
      (grepl("cites", tematica) || grepl("amenazadas", tematica))
    })
    
    is_exotica <- reactive({
      tematica <- r$sel_tematica
      if(is.null(tematica)) return(FALSE)
      tematica %in% c("exoticas_total", "exoticas", "invasoras", "riesgo_invasion")
    })
    
    # Control visibility is now handled centrally in app2.R
    # This module just uses r$show_subcategoria and r$show_especies_total_estimadas
    
    # Generate dynamic controls UI
    output$dynamic_controls <- renderUI({
      controls <- list()
      
      # Subcategory controls (from original app lines 441-456)
      if(r$show_subcategoria) {
        if(grepl("amenazadas", r$sel_tematica)) {
          controls <- append(controls, list(
            selectInput(ns("amenazadas_categoria"), "Categoría Amenaza",
                       choices = c("Total amenazadas" = "_total", 
                                 "EN" = "_en", 
                                 "CR" = "_cr", 
                                 "VU" = "_vu"),
                       selected = "_total")
          ))
        } else if(grepl("cites", r$sel_tematica)) {
          controls <- append(controls, list(
            selectInput(ns("cites_categoria"), "Categoría CITES",
                       choices = c("Total cites" = "_total", 
                                 "I" = "_i", 
                                 "I/II" = "_i_ii", 
                                 "II" = "_ii", 
                                 "III" = "_iii"),
                       selected = "_total")
          ))
        } else if(grepl("exoticas_total", r$sel_tematica)) {
          controls <- append(controls, list(
            selectInput(ns("exoticas_categoria"), "Categoría Exóticas",
                       choices = c("Total" = "_total"),
                       selected = "_total")
          ))
        }
      }
      
      # Species total/estimadas control (from original app lines 457-463)
      if(r$show_especies_total_estimadas) {
        controls <- append(controls, list(
          selectInput(ns("especies_total_estimadas"), "Total o Estimadas",
                     choices = c("Total" = "total", "Estimadas" = "estimadas"),
                     selected = "total")
        ))
      }
      
      # Return controls or NULL
      if(length(controls) > 0) {
        do.call(tagList, controls)
      } else {
        NULL
      }
    })
    
    # Update reactive values when controls change
    observeEvent(input$amenazadas_categoria, {
      r$amenazadas_categoria <- input$amenazadas_categoria
      message("Amenazadas categoria changed to: ", input$amenazadas_categoria)
    })
    
    observeEvent(input$cites_categoria, {
      r$cites_categoria <- input$cites_categoria
      message("CITES categoria changed to: ", input$cites_categoria)
    })
    
    observeEvent(input$exoticas_categoria, {
      r$exoticas_categoria <- input$exoticas_categoria
      message("Exoticas categoria changed to: ", input$exoticas_categoria)
    })
    
    observeEvent(input$especies_total_estimadas, {
      r$especies_total_estimadas <- input$especies_total_estimadas
      message("Especies total/estimadas changed to: ", input$especies_total_estimadas)
    })
    
    # Update current subcategory tracker (from original app lines 1058-1085)
    observe({
      old_value <- r$current_subcategory
      
      if (!is.null(input$amenazadas_categoria)) {
        r$current_subcategory <- input$amenazadas_categoria
      } else if (!is.null(input$cites_categoria)) {
        r$current_subcategory <- input$cites_categoria
      } else {
        r$current_subcategory <- NULL
      }
      
      if(!identical(old_value, r$current_subcategory)) {
        message("Current subcategory changed from ", 
                ifelse(is.null(old_value), "NULL", old_value),
                " to ",
                ifelse(is.null(r$current_subcategory), "NULL", r$current_subcategory))
      }
    })
    
  })
}

#' Update Indicator Based on Data Controls
#' 
#' Updates the indicator value based on selected data controls
#' Based on original app logic (lines 488-534)
#'
#' @param r Reactive values object
#' @return Updated indicator value
update_indicator_from_controls <- function(r) {
  message("=== Updating indicator from controls ===")
  
  # Reset indicators
  old_indicador <- r$indicador
  r$indicador <- NULL
  
  # Only update indicator for maps
  if(r$chart_type == "map") {
    message("Chart type is map - computing indicator...")
    
    # Special themes (amenazadas, cites, exoticas)
    if(!is.null(r$sel_tematica) && 
       (grepl("amenazadas", r$sel_tematica) || grepl("cites", r$sel_tematica))) {
      
      if(grepl("amenazadas", r$sel_tematica)) {
        if(!is.null(r$amenazadas_categoria)) {
          r$indicador <- paste0(r$sel_tipo, "_", r$sel_tematica, r$amenazadas_categoria)
          message("Amenazadas indicator: ", r$indicador)
        }
      }
      
      if(grepl("cites", r$sel_tematica)) {
        if(!is.null(r$cites_categoria)) {
          r$indicador <- paste0(r$sel_tipo, "_", r$sel_tematica, r$cites_categoria)
          message("CITES indicator: ", r$indicador)
        }
      }
      
    } else {
      # Non-special themes
      if(r$sel_tipo == "especies" && is.null(r$sel_tematica)) {
        if(!is.null(r$especies_total_estimadas)) {
          r$indicador <- paste0(r$sel_tipo, "_region_", r$especies_total_estimadas)
          message("Especies total/estimadas indicator: ", r$indicador)
        }
      }
    }
    
    # Handle exoticas themes
    if(!is.null(r$sel_tematica) && 
       r$sel_tematica %in% c("exoticas_total", "exoticas", "invasoras", "riesgo_invasion")) {
      
      tematica <- r$sel_tematica
      if(tematica %in% c("invasoras", "riesgo_invasion")) tematica <- "exoticas"
      
      indicador <- paste0(r$sel_tipo, "_", tematica)
      if(r$sel_tematica == "riesgo_invasion") {
        indicador <- paste0(r$sel_tipo, "_exoticas_", r$sel_tematica)
      }
      
      r$indicador <- indicador
      r$exotica_categoria <- r$sel_tematica
      message("Exoticas indicator: ", r$indicador)
    }
  }
  
  # Reset especies_total_estimadas for special themes
  if(!is.null(r$sel_tematica) && 
     (grepl("amenazadas", r$sel_tematica) || grepl("cites", r$sel_tematica))) {
    r$especies_total_estimadas <- NULL
  }
  
  message("Indicator updated from '", old_indicador, "' to '", r$indicador, "'")
  return(r$indicador)
}

#' Get Subcategory Display Name
#' 
#' Gets the display name for a subcategory value
#'
#' @param subcategory Subcategory value
#' @param theme Theme type ("amenazadas", "cites", "exoticas")
#' @return Display name for subcategory
get_subcategory_display_name <- function(subcategory, theme) {
  if(is.null(subcategory)) return(NULL)
  
  if(theme == "amenazadas") {
    switch(subcategory,
      "_total" = "Total amenazadas",
      "_en" = "EN",
      "_cr" = "CR", 
      "_vu" = "VU",
      subcategory
    )
  } else if(theme == "cites") {
    switch(subcategory,
      "_total" = "Total cites",
      "_i" = "I",
      "_ii" = "II",
      "_iii" = "III",
      "_i_ii" = "I/II",
      subcategory
    )
  } else if(theme == "exoticas") {
    switch(subcategory,
      "_total" = "Total",
      subcategory
    )
  } else {
    subcategory
  }
}