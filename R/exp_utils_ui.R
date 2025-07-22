# exp_utils_ui.R
# UI helper functions for SIB Data App (modular version)

#' Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Database connection helper
#' @param path Path to database file (optional)
#' @return DBI connection to SQLite database
#' @export
get_app_connection <- function(path = NULL) {
  if(is.null(path)){
    path <- sibdata:::sys_file_sibdata("db/sibdata.sqlite")
  }
  
  # Debug: Print database path information
  message("🗄️ Database connection info:")
  message("- Requested path: ", if(is.null(path)) "NULL (using default)" else path)
  message("- Resolved path: ", path)
  message("- File exists: ", file.exists(path))
  if(file.exists(path)) {
    message("- File size: ", file.size(path), " bytes")
  }
  
  DBI::dbConnect(RSQLite::SQLite(),
                 path,
                 read_only = TRUE)
}

#' Get available options for dropdowns
#' @param con Database connection
#' @return List of options for UI inputs
#' @export
get_app_options <- function(con) {
  # Biological groups with hierarchy
  gru <- sibdata_grupo(con) |>
    dplyr::collect() |>
    dplyr::filter(tipo == "biologico")

  gru_tree <- data.tree::FromDataFrameNetwork(gru)
  gru_df <- data.tree::ToDataFrameNetwork(gru_tree,
                                          direction = "descend",
                                          "label", "level", "path")

  paste_dash <- function(str, times = 1) {
    paste(" ", paste0(rep("-", times-1), collapse = ""), str)
  }

  opt_gru <- gru_df |>
    dplyr::rowwise() |>
    dplyr::mutate(label = paste_dash(label, level)) |>
    dplyr::arrange(path)

  opts_grupo_biologico <- opt_gru$from
  names(opts_grupo_biologico) <- opt_gru$label
  opts_grupo_biologico <- c("Todos" = "todos", opts_grupo_biologico)

  # Interest groups
  av_grupos_int <- sib_available_grupos(tipo = "interes", con = con)
  opts_grupo_interes <- c("Todos" = "todos", av_grupos_int)

  # Regions
  pais <- sib_available_regions(subtipo = "País", con = con)
  departamentos <- sib_available_regions(subtipo = "Departamento", con = con)
  opts_region_raw <- c(pais, sort(departamentos))

  # Remove duplicates - keep only first occurrence of each value
  opts_region <- opts_region_raw[!duplicated(opts_region_raw)]

  message("🔧 Region deduplication:")
  message("  - Raw regions: ", length(opts_region_raw))
  message("  - After deduplication: ", length(opts_region))
  duplicated_values <- opts_region_raw[duplicated(opts_region_raw)]
  if (length(duplicated_values) > 0) {
    message("  - Removed duplicates: ", paste(unique(duplicated_values), collapse = ", "))
  }

  # Thematic categories
  all_tematicas <- sib_available_tematicas()
  opts_tematicas_ex <- c("cites_i", "cites_ii", "cites_i_ii", "cites_iii",
                         "exoticas_total")
  opts_tematicas <- all_tematicas[!all_tematicas %in% opts_tematicas_ex]
  opts_tematicas <- c(opts_tematicas, "Ninguna" = "todas")

  list(
    region = opts_region,
    grupo_biologico = opts_grupo_biologico,
    grupo_interes = opts_grupo_interes,
    tematicas = opts_tematicas
  )
}

#' Format species data for display
#' @param data Raw species data
#' @return Formatted data frame
format_species_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)

  vars <- c("label", "registros", "url_gbif", "url_cbc", "kingdom",
            "phylum", "class", "order", "family", "genus")

  data |>
    dplyr::select(dplyr::any_of(vars)) |>
    dplyr::rename(
      "Especie" = "label",
      "Registros" = "registros",
      "Reino" = "kingdom",
      "GBIF" = "url_gbif",
      "CBC" = "url_cbc",
      "Filo" = "phylum",
      "Clase" = "class",
      "Orden" = "order",
      "Familia" = "family",
      "Género" = "genus"
    )
}

#' Create DT options with custom styling
#' @return List of DT options
get_species_table_options <- function() {
  list(
    dom = 'Bftsp',
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    scrollX = TRUE,
    fixedColumns = TRUE,
    fixedHeader = TRUE,
    searching = FALSE,
    info = FALSE,
    pageLength = 10,
    initComplete = htmlwidgets::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#4ad3ac', 'color': '#ffffff'});",
      "}"
    )
  )
}

#' Helper for UI dividers
divider <- function() {
  tags$hr()
}
