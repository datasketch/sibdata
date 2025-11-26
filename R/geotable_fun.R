#' Functions from the geotable package used in sibdata
#'
#' This file contains functions from the `geotable` package that are used in
#' the `sibdata` code, transcribed so they can be used without directly
#' accessing the geotable package.
#'
#' @name geotable_functions
#' @keywords internal
NULL

# Helper functions

#' List tables in DuckDB connection
#'
#' @param con DuckDB connection.
#'
#' @return Character vector of table names.
#'
#' @keywords internal
duckdb_list_tables <- function(con) {
  DBI::dbListTables(con)
}

#' Read table from DuckDB
#'
#' @param tblname Table name.
#' @param con DuckDB connection (optional, defaults to `NULL`).
#' @param collect Logical, whether to collect the result (defaults to `FALSE`).
#'
#' @return Table object (tbl or data frame).
#'
#' @keywords internal
duckdb_read_table <- function(tblname, con = NULL, collect = FALSE) {
  d <- dplyr::tbl(con, tblname)
  if (collect) {
    d <- dplyr::collect(d)
  }
  d
}

#' Read geotable from DuckDB
#'
#' @param tblname Table name.
#' @param con DuckDB connection (optional, defaults to `NULL`).
#' @param geometrycol Name of geometry column (defaults to "geom").
#'
#' @return `sf` object.
#'
#' @keywords internal
duckdb_read_geotable <- function(tblname, con = NULL, geometrycol = "geom") {
  duckdbits::duckdb_load_ext("spatial", con)
  # ST_AsWKB is a DuckDB spatial function, not an R function
  sql <- dbplyr::sql_render(
    dplyr::mutate(
      dplyr::tbl(con, tblname),
      geom = ST_AsWKB(geom) # nolint: object_usage_linter
    )
  )
  sf::st_read(con, query = sql, geometry_column = geometrycol)
}

#' Get main map name from map name
#'
#' @param map_name Map name.
#'
#' @return Main map name.
#'
#' @keywords internal
which_main_map <- function(map_name) {
  gsub("_(.*?)_.*", "_\\1", map_name)
}

#' Check if map is a main map
#'
#' @param map_name Map name.
#'
#' @return Logical indicating if it's a main map.
#'
#' @keywords internal
is_main_map <- function(map_name) {
  map_name == which_main_map(map_name)
}

#' Get available main maps
#'
#' @param con Connection (optional, defaults to `NULL`).
#'
#' @return Character vector of main map names.
#'
#' @keywords internal
gt_available_main_maps <- function(con = NULL) {
  con <- gt_con(con)
  tables <- duckdb_list_tables(con)
  available <- tables[grepl("_sf$", tables)]
  gsub("_sf$", "", available)
}

#' Get available region maps
#'
#' @param con Connection (optional, defaults to `NULL`).
#'
#' @return Character vector of region map names.
#'
#' @keywords internal
gt_available_region_maps <- function(con = NULL) {
  con <- gt_con(con)
  region_maps <- duckdb_read_table("gt_regions_meta", collect = TRUE, con = con)
  region_maps$map_name
}

#' Get all available maps
#'
#' @param con Connection (optional, defaults to `NULL`).
#'
#' @return Character vector of all available map names.
#'
#' @keywords internal
gt_available_maps <- function(con = NULL) {
  con <- gt_con(con)
  main <- gt_available_main_maps(con = con)
  region <- gt_available_region_maps(con = con)
  c(main, region)
}

#' Validate map name
#'
#' @param map_name Map name to validate.
#' @param con Connection (optional, defaults to `NULL`).
#'
#' @return Invisible, throws error if map name is not valid.
#'
#' @keywords internal
gt_valid_map_name <- function(map_name, con = NULL) {
  con <- gt_con(con)
  if (!map_name %in% gt_available_maps(con = con)) {
    stop(map_name, " not available, check `available_maps()`")
  }
}

#' Get regions for a map
#'
#' @param map_name Map name (optional, defaults to `NULL`).
#' @param con Connection.
#'
#' @return Table with region information.
#'
#' @keywords internal
gt_regions <- function(map_name = NULL, con) {
  regs <- duckdb_read_table("gt_regions", con = con)
  if (!is.null(map_name)) {
    nm <- map_name
    regs <- dplyr::filter(regs, map_name == nm)
  }
  regs
}

# Main functions

#' Connection to the geotable database
#'
#' @param con Existing connection (optional, defaults to `NULL`).
#' @param read_only Logical, indicates if the connection is read-only (defaults
#'   to `TRUE`).
#'
#' @return Connection object to the geotable database.
#'
#' @export
gt_con <- function(con = NULL, read_only = TRUE) {
  if (is.null(con)) {
    dbdir <- sys_file_sibdata("db/geotable.duckdb")
    con <- duckdbits::duckdb_con(dbdir = dbdir, read_only = read_only)
  }
  if (is.character(con)) {
    con <- duckdbits::duckdb_con(dbdir = con, read_only = read_only)
  }
  con
}

#' Get Simple Features (sf) object from a map
#'
#' @param map_name Name of the map to retrieve.
#' @param con Connection to the geotable database (optional, defaults to
#'   `NULL`).
#'
#' @return `sf` object with the geographic data of the requested map.
#'
#' @keywords internal
gt_sf <- function(map_name, con = NULL) {
  con <- gt_con(con)
  gt_valid_map_name(map_name, con)
  main_map <- gsub("_(.*?)_.*", "_\\1", map_name)
  sf <- duckdb_read_geotable(paste0(main_map, "_sf"), con)
  if (is_main_map(map_name)) {
    return(sf::st_set_crs(sf, 4326))
  }
  region_geographies <- dplyr::collect(
    dplyr::select(gt_regions(map_name, con), "id")
  )
  sf <- dplyr::inner_join(sf, region_geographies, by = "id")
  sf::st_set_crs(sf, 4326)
}

#' Rename columns with double dots
#'
#' @param x `sf` object containing columns with names starting with "..".
#'
#' @return `sf` object with renamed columns.
#'
#' @keywords internal
rename_dotdot <- function(x) {
  no_dotdot_idx <- !grepl("^\\.\\.gt|geom", names(x))
  names(x)[no_dotdot_idx] <- paste0("..gt_", names(x)[no_dotdot_idx])
  x
}

#' Disconnect geotable connection
#'
#' @param con Connection to the geotable database to disconnect.
#'
#' @return `NULL` (invisible).
#'
#' @keywords internal
gt_discon <- function(con) {
  duckdbits::duckdb_disconnect(con)
}

#' Default options for map icons
#'
#' @return List with default options for icons.
#'
#' @keywords internal
default_icon_opts <- function() {
  list(
    main_border_color = "#888888",
    main_border_width = 1.5,
    fill_color = "#f0f0f9",
    minor_border_color = "#aaaaaa",
    minor_border_width = 0.5,
    background_color = "transparent",
    projection = "wgs84",
    save_width = 2,
    save_height = 2,
    simplify = FALSE,
    outline = FALSE,
    keep = 0.05,
    islands = NULL,
    union_percentage = NULL
  )
}

#' Default cartographic projections
#'
#' @return List with default projections.
#'
#' @keywords internal
default_projections <- function() {
  list(
    wgs84 = list(
      name = "WGS84",
      crs = sf::st_crs(4326),
      epsg = 4326
    ),
    robin = list(
      name = "Robinson",
      crs = "+proj=robin"
    ),
    eck4 = list(
      name = "Winkle Triple",
      crs = "+proj=eck4"
    ),
    moll = list(
      name = "Mollweide",
      crs = "+proj=moll"
    ),
    merc = list(
      name = "Mercator",
      crs = "+proj=merc"
    )
  )
}

#' ggplot theme without visual elements
#'
#' @param background_color Background color of the plot (defaults to
#'   "transparent").
#'
#' @return ggplot theme object (`theme`).
#'
#' @keywords internal
gg_theme_nothing <- function(background_color = "transparent") {
  if (background_color == "transparent") {
    bg <- ggplot2::element_blank()
  } else {
    bg <- ggplot2::element_rect(fill = background_color, color = NA)
  }
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    plot.background = bg,
    panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks.length = grid::unit(0, "cm"),
    panel.spacing = grid::unit(0, "lines"),
    plot.margin = grid::unit(c(0, 0, 0, 0), "lines")
  )
}
