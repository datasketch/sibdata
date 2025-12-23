#' Functions from the geotable package used in sibdata
#'
#' This file contains functions from the `geotable` package that are used in
#' the `sibdata` code, transcribed so they can be used without directly
#' accessing the geotable package.
#'
#' @name geotable_functions
#' @keywords internal
NULL

# DuckDB helper functions (internal replacements for duckdbits)

#' Load DuckDB extension
#'
#' @param ext_name Name of the extension to load (e.g., "spatial").
#' @param con DuckDB connection.
#'
#' @return Invisible, loads the extension.
#'
#'@export
duckdb_load_ext <- function(ext_name, con) {
  # Try to load the extension, install if needed
  tryCatch(
    {
      DBI::dbExecute(con, paste0("LOAD ", ext_name, ";"))
    },
    error = function(e) {
      # If load fails, try to install first
      DBI::dbExecute(con, paste0("INSTALL ", ext_name, ";"))
      DBI::dbExecute(con, paste0("LOAD ", ext_name, ";"))
    }
  )
  invisible(NULL)
}

#' Create DuckDB connection
#'
#' @param dbdir Path to DuckDB database file.
#' @param read_only Logical, indicates if the connection is read-only (defaults
#'   to `TRUE`).
#'
#' @return DuckDB connection object.
#'
#' @export
duckdb_con <- function(dbdir, read_only = TRUE) {
  drv <- duckdb::duckdb(dbdir)
  DBI::dbConnect(drv, read_only = read_only)
}

#' Disconnect DuckDB connection
#'
#' @param con DuckDB connection to disconnect.
#'
#' @return `NULL` (invisible).
#'
#' @export
duckdb_disconnect <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(NULL)
}

# Helper functions

#' List tables in DuckDB connection
#'
#' @param con DuckDB connection.
#'
#' @return Character vector of table names.
#'
#' @export
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
#' @export
duckdb_read_geotable <- function(tblname, con = NULL, geometrycol = "geom") {
  duckdb_load_ext("spatial", con)
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
#' @export
which_main_map <- function(map_name) {
  gsub("_(.*?)_.*", "_\\1", map_name)
}

#' Check if map is a main map
#'
#' @param map_name Map name.
#'
#' @return Logical indicating if it's a main map.
#'
#' @export
is_main_map <- function(map_name) {
  map_name == which_main_map(map_name)
}

#' Get available main maps
#'
#' @param con Connection (optional, defaults to `NULL`).
#'
#' @return Character vector of main map names.
#'
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
    con <- duckdb_con(dbdir = dbdir, read_only = read_only)
  }
  if (is.character(con)) {
    con <- duckdb_con(dbdir = con, read_only = read_only)
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
#' @export
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
#' @export
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
#' @export
gt_discon <- function(con) {
  duckdb_disconnect(con)
}

#' Default options for map icons
#'
#' @return List with default options for icons.
#'
#' @export
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
#' @export
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
#' @export
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




#' Match geotable Data to a geotable Map
#'
#' This function matches input data to a geotable map, based on the provided map_name
#' and the specified column.
#'
#' @param d The input data frame to be matched.
#' @param map_name The name of the geotable map.
#' @param col The column name or index to be used for matching. If NULL, the function
#' will try to guess the appropriate column for matching.
#' @param centroids A logical value indicating whether to include centroids in the
#'  output. Defaults to TRUE.
#' @return A data frame with the matched data and centroids if requested with ..gd_id and ..gd_name.
#' @examples
#' data <- readr::read_csv("data-raw/sample_data/col_departments/plebiscito_2016_departamentos.csv")
#' gd_match(data, map_name = "col_departments")
#' @export
gt_match <- function(d, map_name, col = NULL, centroids = TRUE,
                     unique = FALSE,
                     con = NULL,
                     threshold = 0.8,
                     strict_threshold = TRUE){

  con <- gt_con(con = con)
  gt_valid_map_name(map_name, con = con)

  if(is.null(d)) return(NULL)
  out <- NULL

  if(is.null(col)){
    #if(length(col == 2)) stop("TODO: auto match parent_geography")
    geocode_col <- gt_which_geocode_col(
      d, map_name, con = con,
      threshold = threshold,
      strict_threshold = strict_threshold
    )

    if(!is.na(geocode_col)){
      # Guess first which is the geocode col
      out <- gd_match_codes(d, map_name = map_name, col = geocode_col, con = con)
    } else{
      # If none found try names
      geoname_col <- gt_which_geoname_col(
        d, map_name, con = con,
        threshold = threshold,
        strict_threshold = strict_threshold
      )
      if(!all(is.na(geoname_col))){
        if(length(geoname_col) == 1){
          out <- gt_match_names(d, map_name = map_name, col = geoname_col, con = con)
        }
        if(length(geoname_col) == 2){
          out <- gt_match_names2(d, map_name = map_name, col = geoname_col, con = con)
        }
      }
    }
  } else{
    col <- parse_col(d, col)
    if(length(col) == 1){
      code_or_name <- is_code_or_name(d[[col]], map_name)
      if(code_or_name == "code"){
        out <- gt_match_codes(d, map_name = map_name, col = col, con = con)
      }
      if(code_or_name == "name"){
        out <- gt_match_names(d, map_name = map_name, col = col, con = con)

      }
    }
    if(length(col) == 2){
      out <- gt_match_names2(d, map_name = map_name, col = col, con = con)
    }
  }

  if(centroids){
    centroids <- gt_centroids(map_name, con = con) |>
      collect() |>
      rename_dotdot()
    by <- c("..gt_id")
    out <- out |> dplyr::left_join(centroids, by = by)
  }

  if(unique){
    out <- out |>
      distinct(..gt_id, .keep_all = TRUE)
  }

  out
}



#' Match geotable Data to a geotable Map by Codes
#'
#' This function matches input data to a geotable map based on the provided map_name and the specified code column.
#'
#' @param d The input data frame to be matched.
#' @param map_name The name of the geotable map. Defaults to NULL.
#' @param col The column name or index to be used for matching by codes. Defaults to NULL.
#' @return A data frame with the matched data based on codes.
#' @examples
#' data <- data.frame(id_depto = c("05", "08", "81", "11"), value = runif(4))
#' gd_match_codes(data, map_name = "col_departments")
#' gd_match_codes(data, map_name = "col_departments", "id_depto")
#' @export
gd_match_codes <- function(d, map_name = NULL, col = NULL, con = NULL){
  con <- gt_con(con)
  if(is.null(map_name)){
    stop("Need a map_name to match")
  }
  col <- parse_col(d, col)

  join_by <- "id"
  names(join_by) <- col

  codes <- gt_codes(map_name, con = con) |> collect() |> rename_dotdot()
  codes$id <- codes$..gt_id

  # Quick fix when the codes in input table are numbers
  if(is.character(codes$id)){
    d[[col]] <- as.character(d[[col]])
  }

  if(grepl("col_departments", map_name)){
    # d[[col]] <- leading_zeros(d[[col]], 2)
    d[[col]] <- stringr::str_pad(d[[col]], 2, side = "left", pad = "0")
  }
  if(grepl("col_municipalities", map_name)){
    # d[[col]] <- leading_zeros(d[[col]], 5)
    d[[col]] <- stringr::str_pad(d[[col]], 5, side = "left", pad = "0")
  }

  dplyr::left_join(d, codes, by = join_by)

}


#' Match geotable Data to a geotable Map by Names
#'
#' This function matches input data to a geotable map based on the provided map_name
#'  and the specified name column.
#'
#' @param d The input data frame to be matched.
#' @param map_name The name of the geotable map. Defaults to NULL.
#' @param col Optional column with name to be used for matching by names. Defaults to NULL.
#' @param codes Optional codes data frame to be used for matching. Defaults to NULL.
#' @param altnames Optional alternative names data frame to be used for matching. Defaults to NULL.
#' @return A data frame with the matched data based on names.
#' @examples
#' data <- data.frame(id_name = c("Antioquia", "Bogota", "Quindio", "Caldas"), value = runif(4))
#' gd_match_names(data, map_name = "col_departments")
#' @export
gt_match_names <- function(d, map_name = NULL, col = NULL,
                           codes = NULL, altnames = NULL, con = NULL){
  if(is.null(map_name)){
    stop("Need a map_name to match")
  }
  con <- gt_con(con = con)
  col <- parse_col(d, col)

  altnames <- gt_altnames(map_name, str_clean = TRUE, con = con) |>
    select(-map_name) |>
    rename_dotdot()

  d$..gt_altname <- str_clean(d[[col]])


  match <- dplyr::left_join(d, altnames, by = "..gt_altname", copy = TRUE)
  helper_cols <- c("..gt_altname", "..gt_name","..gt_parent_id", "..gt_parent_name",
                   "..gt_lang", "..gt_region_code", "..gt_region_name",
                   "..gt_priority", "..gt_altname_source",
                   "..gt_level", "..gt_region_id", "..gt_region_type")
  match <- match |> dplyr::select(-any_of(helper_cols))

  ## Match again with original codes and not altnames
  codes <- gt_codes(map_name, con) |> collect() |> rename_dotdot()
  m <- match |> dplyr::left_join(codes, by = "..gt_id", copy = TRUE)
  m
}



#' Get Rows with No Match in geotable Data
#'
#' This function returns the rows in the input data frame that do not match the
#'  specified geotable map.
#'
#' @param d The input data frame to be checked for non-matching rows.
#' @param map_name The name of the geotable map. Defaults to NULL.
#' @param col A column name or index to be used for matching. Defaults to NULL.
#' @return A data frame containing rows with no match in the geotable data.
#' @examples
#' data <- data.frame(geo_name = c("AR", "COL", "PER", "EC", "BLA"), val = runif(5))
#' gd_no_match(data, map_name = "world_countries", col = "geo_name")
#' @export
gt_no_match <- function(d, map_name, col = NULL){
  match <- gt_match(d, map_name, col = col)
  match |> dplyr::filter(is.na(..gt_id))
}


#' Identify the column with geocode data in a dataframe
#'
#' This function takes a dataframe and a map name as inputs, and returns the name
#' of the column that has the geocode data for the specified map. If data have
#' more than 50 rows, the function looks at the first 50 rows of the dataframe
#' and checks if any column has values that match the geocode ids for the given map.
#' The column with the highest number of matching geocode ids is returned, provided
#' that at least a user-specified percentage of the values in that column agree
#' with the geocode.
#'
#' @param d A dataframe containing the data to be analyzed.
#' @param map_name A character string specifying the name of the map to be used,
#' you can view available maps with geotable::available_maps().
#' @return The name of the column with geocode data, or NA if no column meets the
#' 90\% agreement threshold.
#' @examples
#' df <- data.frame(id_country = c("ARG", "COL", "AGO", "BRA"), value = runif(4))
#' which_geocode_col(df, "world_countries")
#' @export
gt_which_geocode_col <- function(d, map_name,
                                 con = NULL,
                                 threshold = 0.8,
                                 strict_threshold = TRUE) {
  con <- gt_con(con)
  # find the column that matches most values in the codes
  gt_valid_map_name(map_name, con = con)
  codes_table <- gt_codes(map_name = map_name, con = con)
  x <- d |>
    slice(1:1000) |>
    mutate_all(as.character) |>
    tidyr::pivot_longer(cols = everything(),
                        names_to = "column", values_to = "value") |>
    distinct()

  data_frame_db <- copy_to(con, x, "temp_data", overwrite = TRUE)
  # Perform the join operation and count the matches for each name
  matches <- data_frame_db |>
    inner_join(codes_table, by = c("value" = "id")) |>
    group_by(column) |>
    summarize(total_matches = n(), .groups = 'drop') |>
    #arrange(desc(total_matches)) |>
    slice_max(total_matches, n = 1) |>
    collect()
  if(nrow(matches) == 0) return(NA)

  # At least a user-specified percentage of values should agree with the geocode
  if(matches$total_matches < threshold * nrow(d)) {
    message(paste0(
      "No column matches with more than ",
      threshold * 100,
      "% of column names"
    ))

    if (strict_threshold) return(NA)
  }
  matches$column
}


#' Identify the column with geoname data in a dataframe
#'
#' This function takes a dataframe and a map name as inputs, and returns the name
#' of the column that has the geoname data for the specified map. If data have
#' more than 50 rows, the function looks at the first 50 rows of the dataframe
#' and checks if any column has values that match the geocode ids for the given map.
#' The column with the highest number of matching geocode ids is returned, provided
#' that at least a user-specified percentage of the values in that column agree
#' with the geoname.
#'
#' @param d A dataframe containing the data to be analyzed.
#' @param map_name A character string specifying the name of the map to be used,
#' you can view available maps with geotable::available_maps().
#' @return The name of the column with geoname data, or NA if no column meets the
#' 90\% agreement threshold.
#' @examples
#' df <- data.frame(country = c("Argentina", "Colombia", "Angora", "Brasil"), value = runif(4))
#' which_geoname_col(df, "world_countries")
#' @export
gt_which_geoname_col <- function(d, map_name,
                                 con = NULL,
                                 threshold = 0.5,
                                 strict_threshold = TRUE) {
  con <- gt_con(con)
  # find the column that matches most values in the codes
  gt_valid_map_name(map_name, con = con)
  altnames_table <- gt_altnames(map_name = map_name, str_clean = TRUE, con = con)
  x <- d |>
    mutate_all(as.character) |>
    tidyr::pivot_longer(cols = everything(),
                        names_to = "column", values_to = "value") |>
    mutate(value = str_clean(value)) |>
    distinct() |>
    slice(1:1000)

  data_frame_db <- copy_to(con, x, "temp_data", overwrite = TRUE)
  # Perform the join operation and count the matches for each name
  matches <- data_frame_db |>
    inner_join(altnames_table, by = c("value" = "altname"), copy = TRUE) |>
    group_by(column) |>
    summarize(total_matches = n(), .groups = 'drop') |>
    #arrange(desc(total_matches)) |>
    slice_max(total_matches, n = 1) |>
    collect()

  if(nrow(matches) == 0) return(NA)

  # At least a user-specified percentage of values should agree with the geoname
  if(matches$total_matches[1] < threshold * length(unique(d[[matches$column[1]]]))) {
    message(paste0(
      "No column matches with more than ",
      threshold * 100,
      "% of column names"
    ))

    if (strict_threshold) return(NA)
  }
  matches$column[1]
}


#' @export
gt_codes <- function(map_name = NULL, con){
  codes <- duckdb_read_table("gt_codes", con = con)
  nm <- map_name
  main_map <- which_main_map(map_name)
  codes <- codes |> filter(map_name == main_map)
  # filter the codes is they are regions
  if(main_map != nm){
    regs <- gt_regions(map_name = nm, con) |> select(id)
    codes <- codes |> inner_join(regs, by = "id")
  }
  codes
}

#' @export
gt_altnames <- function(map_name = NULL, str_clean = FALSE, con){
  altnames <- duckdb_read_table("gt_altnames", con = con)
  nm <- map_name
  main_map <- which_main_map(map_name)
  altnames <- altnames |> filter(map_name == main_map)
  # filter the codes is they are regions
  if(main_map != nm){
    regs <- gt_regions(map_name = nm, con) |> select(id)
    altnames <- altnames |> inner_join(regs, by = "id")
  }
  codes <- gt_codes(map_name, con) |> rename(altname = name)
  altnames <- union_all(codes,altnames)

  # TODO clean names also in duckdb
  if(str_clean){
    altnames <- altnames |>
      collect() |>
      mutate(altname = str_clean(altname)) |>
      distinct(id, altname,map_name, .keep_all = TRUE)
  }
  altnames
}


#' @export
gt_regions <- function(map_name = NULL, con){
  regs <- duckdb_read_table("gt_regions", con = con)
  if(!is.null(map_name)){
    nm <- map_name
    regs <- regs |> filter(map_name == nm)
  }
  regs
}

#' @export
gt_regions_meta <- function(map_name = NULL, parent_map_name = NULL, con = NULL){
  con <- gt_con(con)
  regs_meta <- duckdb_read_table("gt_regions_meta", con = con)
  if(!is.null(map_name)){
    nm <- map_name
    regs_meta <- regs_meta |> filter(map_name == nm)
  }
  if(!is.null(parent_map_name)){
    nm <- parent_map_name
    regs <- regs_meta |> filter(parent_map_name == nm)
  }
  regs_meta
}

#' @export
gt_centroids <- function(map_name = NULL, con = NULL){
  con <- gt_con(con)
  cents <- duckdb_read_table("gt_centroids", con = con)
  nm <- map_name
  main_map <- which_main_map(map_name)
  cents <- cents |> filter(map_name == main_map)
  # filter the codes is they are regions
  if(main_map != nm){
    regs <- gt_regions(map_name = nm, con) |> select(id)
    cents <- cents |> inner_join(regs, by = "id")
  }
  cents |> select(-map_name)
}
