# exp_utils_ui.R
# UI helper functions for SIB Data App (modular version)

#' Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Database connection helper
#' @param path Path to database file (optional)
#' @param debug Boolean to control console debug output
#' @return DBI connection to SQLite database
#' @export
get_app_connection <- function(path = NULL, debug = FALSE) {
  # Resolve default database path if not provided
  if (is.null(path)) {
    duckdb_default <- sibdata:::sys_file_sibdata("db/sibdata.duckdb")
    sqlite_default <- sibdata:::sys_file_sibdata("db/sibdata.sqlite")
    path <- if (file.exists(duckdb_default)) duckdb_default else sqlite_default
  }

  # Determine engine by file extension
  is_duckdb <- grepl("\\.duckdb$", path, ignore.case = TRUE)

  # Debug: Print database path and engine information
  if (debug) {
    message("🗄️ Database connection info:")
    message("- Resolved path: ", path)
    message("- File exists: ", file.exists(path))
    message("- Engine: ", if (is_duckdb) "DuckDB" else "SQLite")
    if (file.exists(path)) {
      message("- File size: ", file.size(path), " bytes")
    }
  }

  if (is_duckdb) {
    DBI::dbConnect(duckdb::duckdb(),
                   path,
                   read_only = TRUE)
  } else {
    DBI::dbConnect(RSQLite::SQLite(),
                   path,
                   read_only = TRUE)
  }
}

#' Get available options for dropdowns
#' @param con Database connection
#' @param debug Boolean to control console debug output
#' @return List of options for UI inputs
#' @export
get_app_options <- function(con, debug = FALSE) {
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

  # Regions - grouped by type
  pais <- sib_available_regions(subtipo = "País", con = con)
  departamentos <- sib_available_regions(subtipo = "Departamento", con = con)
  especial <- sib_available_regions(subtipo = "Especial", con = con)

  # Create grouped options for selectize
  opts_region_grouped <- list(
    "Colombia" = pais,
    "Departamentos" = sort(departamentos),
    "Especial" = sort(especial)
  )

  # Create flat list for backward compatibility and type detection
  opts_region_raw <- c(pais, sort(departamentos), sort(especial))
  opts_region <- opts_region_raw[!duplicated(opts_region_raw)]

  # Create individual lists for type detection
  region_colombia <- pais
  region_departamentos <- sort(departamentos)
  region_especial <- sort(especial)

  if (debug) {
    message("🔧 Region grouping:")
    message("  - Colombia regions: ", length(region_colombia))
    message("  - Departamentos: ", length(region_departamentos))
    message("  - Especial: ", length(region_especial))
    message("  - Total unique regions: ", length(opts_region))
  }

  # Thematic categories
  all_tematicas <- sib_available_tematicas()
  opts_tematicas_ex <- c("cites_i", "cites_ii", "cites_i_ii", "cites_iii",
                         "exoticas","invasoras","exoticas_riesgo_invasion",
                         "trasplantadas")
  opts_tematicas <- all_tematicas[!all_tematicas %in% opts_tematicas_ex]
  opts_tematicas <- c(opts_tematicas, "Ninguna" = "todas")

  list(
    region = opts_region,
    region_grouped = opts_region_grouped,
    region_colombia = region_colombia,
    region_departamentos = region_departamentos,
    region_especial = region_especial,
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
  if("tematica_label" %in% names(data)){
    vars <- c(vars, "tematica_label")
  }
  formatted_data <- data |>
    dplyr::select(dplyr::any_of(vars)) |>
    dplyr::rename(
      "Especie" = "label",
      "Observaciones" = "registros",
      "Reino" = "kingdom",
      "GBIF" = "url_gbif",
      "CBC" = "url_cbc",
      "Filo" = "phylum",
      "Clase" = "class",
      "Orden" = "order",
      "Familia" = "family",
      "Género" = "genus"
    )

  # Only rename and relocate tematica_label if it exists
  if("tematica_label" %in% names(data)) {
    formatted_data <- formatted_data |>
      dplyr::rename("Tematica" = "tematica_label") |>
      dplyr::relocate(dplyr::all_of("Tematica"), .after = "Observaciones")
  }

  formatted_data
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
