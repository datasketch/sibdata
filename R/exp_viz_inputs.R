# exp_viz_inputs.R
# Visualization Inputs Module: computes tematica, indicador, breadcrumb
# and exposes UI to control tipo and chart type selection.

#' Visualization Inputs UI Module
#'
#' Provides controls for visualization type (tipo) and chart selector,
#' and displays breadcrumb.
#'
#' @param id Module ID
#' @export
exp_viz_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "text-align: center; margin-bottom: 10px;",
        uiOutput(ns("chart_selector_container"))
    ),
    div(style = "margin-top: 10px;",
        selectInput(ns("sel_tipo"), "Tipo",
                    choices = c("Observaciones" = "registros",
                                "Especies" = "especies"),
                    selected = "registros")
    ),
    hr(),
    # Breadcrumb acts as chart title - no label needed
    div(style = "font-weight: 600; font-size: 16px; margin-bottom: 10px;",
        textOutput(ns("breadcrumb"))
    )
  )
}

#' Visualization Inputs Server Module
#'
#' Syncs tipo and chart type with reactive values, computes tematica,
#' indicador, and breadcrumb, and manages chart availability.
#'
#' @param id Module ID
#' @param r Reactive values object (shared)
#' @param debug Boolean to control console debug output
#' @export
exp_viz_inputs_server <- function(id, r, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Keep tipo in sync with r
    observeEvent(input$sel_tipo, {
      r$sel_tipo <- input$sel_tipo
      if (debug) message("🎛️ TIPO updated → ", r$sel_tipo)
    }, ignoreInit = FALSE)

    # Compute tematica and indicador
    # Use priority to ensure this runs before data fetching
    observe({
      # Explicitly read all inputs that affect indicador calculation
      sel_tipo <- r$sel_tipo
      sel_tematica <- r$sel_tematica
      sel_subtematica <- r$sel_subtematica
      amenazadas_categoria <- r$amenazadas_categoria
      chart_type <- r$chart_type
      
      # tematica slug from selection
      r$tematica <- if (!is.null(sel_tematica)) gsub("-", "_", sel_tematica) else sel_tematica

      # indicador based on current state
      r$indicador <- calculate_indicador_viz(r)

      if (debug) {
        message("🔧 VIZ INPUTS COMPUTE:")
        message("  - sel_tipo: ", sel_tipo)
        message("  - sel_tematica: ", sel_tematica)
        message("  - sel_subtematica: ", sel_subtematica)
        message("  - amenazadas_categoria: ", amenazadas_categoria)
        message("  - chart_type: ", chart_type)
        message("  - tematica: ", r$tematica)
        message("  - indicador: ", r$indicador)
      }
    }, priority = 10)

    # Compute available charts and ensure a valid active chart
    observe({
      # All chart types available (Map first, Cards second)
      all_charts <- c("Mapa" = "map", "Tarjetas" = "cards", "Torta" = "pie", "Dona" = "donut",
                      "Treemap" = "treemap", "Barras" = "bar")
      map_table <- c("Mapa" = "map", "Tarjetas" = "cards")

      # Amenazadas total selection
      is_amenazadas_total <- !is.null(r$sel_tematica) &&
        grepl("amenazadas", r$sel_tematica) &&
        !is.null(r$amenazadas_categoria) && r$amenazadas_categoria == "_total"

      # Special regions where map disabled
      special_regions_disable_map <- c(
        "region-amazonia",
        "reserva-forestal-la-planada",
        "resguardo-indigena-pialapi-pueblo-viejo",
        "bogota-dc"
      )
      is_special_region_selected <- !is.null(r$sel_region) && r$sel_region %in% special_regions_disable_map
      if (isTRUE(is_special_region_selected) != isTRUE(r$is_special_region)) {
        r$is_special_region <- is_special_region_selected
      }

      # Allow full chart set only for specific tematicas when no subtematica
      allow_full_charts <- FALSE
      if (!is.null(r$tematica)) {
        allow_full_charts <- is.null(r$sel_subtematica) && r$tematica %in% c("amenazadas_global", "amenazadas_nacional", "cites")
      }

      computed_available <- NULL
      if (!is.null(r$sel_tematica) && grepl("exoticas", r$sel_tematica)) {
        computed_available <- map_table
      } else if (allow_full_charts || is_amenazadas_total) {
        computed_available <- all_charts
      } else {
        computed_available <- map_table
      }

      if (is_special_region_selected) {
        computed_available <- computed_available[computed_available != "map"]
      }

      if (is.null(r$available_charts) || !identical(unname(r$available_charts), unname(computed_available))) {
        r$available_charts <- computed_available
      }

      # Ensure active chart is available; prefer map, else cards, else first
      if (is.null(r$chart_type) || !r$chart_type %in% r$available_charts) {
        av_values <- unname(r$available_charts)
        default_chart <- if ("map" %in% av_values) "map" else if ("cards" %in% av_values) "cards" else av_values[1]
        if (!identical(r$chart_type, default_chart)) {
          r$chart_type <- default_chart
          if (debug) message("✓ VIZ chart_type set to default: ", default_chart)
        }
      }

      if (debug) {
        message("🎨 AVAILABLE CHARTS: ", paste(names(r$available_charts), collapse = ", "))
        message("🎨 ACTIVE CHART: ", r$chart_type)
      }
    })

    # Render chart selector
    output$chart_selector_container <- renderUI({
      # All chart types available
      all_charts <- c("Mapa" = "map", "Tarjetas" = "cards", "Torta" = "pie", "Dona" = "donut",
                      "Treemap" = "treemap", "Barras" = "bar")

      av_charts <- if (!is.null(r$available_charts) && length(r$available_charts) > 0) r$available_charts else c("Mapa" = "map", "Tarjetas" = "cards")
      active_chart <- if (!is.null(r$chart_type) && r$chart_type %in% av_charts) r$chart_type else av_charts[1]

      if (is.null(r$chart_type) || !r$chart_type %in% av_charts) {
        r$chart_type <- active_chart
      }

      shinyinvoer::buttonImageInput(
        inputId = ns('chart_type'),
        images = all_charts,
        highlightColor = "#09A274",
        button_width = 28,
        path = 'www/viz_icons',
        active = active_chart,
        layout = "flex",
        disabled = all_charts[!all_charts %in% av_charts]
      )
    })

    # Sync chart_type back to r
    observeEvent(input$chart_type, {
      if (!is.null(input$chart_type)) {
        r$chart_type <- input$chart_type
        if (debug) message("📊 CHART TYPE changed → ", r$chart_type)
      }
    }, ignoreInit = TRUE)

    # Breadcrumb - explicitly read all dependencies
    observe({
      # Explicitly read all reactive values that affect breadcrumb
      sel_region <- r$sel_region
      sel_tipo <- r$sel_tipo
      sel_tematica <- r$sel_tematica
      sel_subtematica <- r$sel_subtematica
      amenazadas_categoria <- r$amenazadas_categoria
      tematica <- r$tematica
      sel_grupo <- r$sel_grupo
      
      # Now compute breadcrumb with fresh values
      r$breadcrumb <- create_breadcrumb_viz(r)
      
      if (debug) {
        message("🔖 BREADCRUMB updated: ", r$breadcrumb)
      }
    }, priority = 9)

    output$breadcrumb <- renderText({
      req(r$breadcrumb)
      r$breadcrumb
    })
  })
}


# Helpers (local to this module)

calculate_indicador_viz <- function(r){
  regs_or_esps <- r$sel_tipo
  tematica <- if (!is.null(r$sel_tematica)) gsub("-", "_", r$sel_tematica) else r$sel_tematica

  if (is.null(tematica)) {
    # Default especies_total_estimadas to "total" if not set
    especies_est <- r$especies_total_estimadas %||% "total"
    indicador <- dplyr::case_when(
      r$sel_tipo == "especies" && identical(especies_est, "total") ~ "especies_region_total",
      r$sel_tipo == "especies" && identical(especies_est, "estimadas") ~ "especies_region_estimadas",
      TRUE ~ "registros_region_total"
    )
  } else if (!is.null(r$sel_tematica) && grepl("exoticas", r$sel_tematica)) {
    if (r$sel_tematica == "exoticas-total") {
      indicador <- NULL
      if (!is.null(r$sel_subtematica)) {
        sub_slug <- gsub("-", "_", r$sel_subtematica)
        indicador <- glue::glue("{regs_or_esps}_{sub_slug}")
      }
    } else {
      indicador <- glue::glue("{regs_or_esps}_{tematica}")
    }
  } else if (!is.null(r$sel_subtematica) && nzchar(r$sel_subtematica)) {
    sub_slug <- gsub("-", "_", r$sel_subtematica)
    indicador <- glue::glue("{regs_or_esps}_{sub_slug}")
  } else if (!is.null(r$sel_tematica) && (grepl("amenazadas", r$sel_tematica) || grepl("cites", r$sel_tematica))) {
    if (!is.null(r$chart_type) && r$chart_type == "map") {
      indicador <- glue::glue("{regs_or_esps}_{tematica}_total")
    } else if (isTRUE(r$has_subtematica)) {
      indicador <- NULL
    } else {
      indicador <- glue::glue("{regs_or_esps}_{tematica}")
    }
  } else {
    indicador <- glue::glue("{regs_or_esps}_{tematica}")
  }

  return(indicador)
}

create_breadcrumb_viz <- function(r){
  region <- tools::toTitleCase(gsub("-", " ", r$sel_region))
  tipo_text <- if (r$sel_tipo == "registros") "Observaciones" else "Especies"

  tematica_text <- if (is.null(r$sel_tematica)) {
    "todas las temáticas"
  } else if (!is.null(r$sel_subtematica) && nzchar(r$sel_subtematica)) {
    if (grepl("^cites", r$sel_subtematica)) {
      suf <- sub("^cites[-_]", "", r$sel_subtematica)
      roman <- toupper(gsub("-", " ", suf))
      paste("CITES", roman)
    } else {
      tools::toTitleCase(gsub("-", " ", r$sel_subtematica))
    }
  } else if (!is.null(r$amenazadas_categoria) && r$amenazadas_categoria != "_total" &&
             !is.null(r$tematica) && grepl("amenazadas", r$tematica)) {
    scope <- if (grepl("global", r$tematica)) "categoría global"
             else if (grepl("nacional", r$tematica)) "categoría nacional" else ""
    sub_lab <- toupper(sub("^_", "", r$amenazadas_categoria))
    paste(sub_lab, scope)
  } else {
    tem_slug <- if (!is.null(r$tematica)) r$tematica else gsub("-", "_", r$sel_tematica)
    if (identical(tem_slug, "cites")) {
      "CITES"
    } else {
      tools::toTitleCase(gsub("_", " ", tem_slug))
    }
  }

  grupo_text <- ""
  if (!is.null(r$sel_grupo)) {
    grupo <- tools::toTitleCase(gsub("-", " ", r$sel_grupo))
    grupo_text <- paste("del grupo", grupo)
  }

  breadcrumb <- paste(tipo_text, "para", tematica_text, "en", region, grupo_text)
  return(breadcrumb)
}



