# exp_charts.R
# hgmagic chart integration module for SIB Data App

#' Generate Chart Using hgmagic
#' 
#' Creates charts using hgmagic library based on chart type and data
#' Replicates the functionality from the original app (lines 819-841)
#'
#' @param chart_type Type of chart ("pie", "bar", "donut", "treemap")
#' @param data Data to visualize
#' @param r Reactive values object for additional options
#' @param con Database connection for label processing
#' @return hgmagic chart object
create_hgmagic_chart <- function(chart_type, data, r, con) {
  message("=== Creating hgmagic chart ===")
  message("Chart type: ", chart_type)
  message("Data rows: ", nrow(data))
  message("Data columns: ", paste(names(data), collapse = ", "))
  
  # Prepare data for hgmagic
  chart_data <- prepare_chart_data(data, chart_type, r, con)
  
  # Get chart options
  chart_options <- get_chart_options(chart_type, r)
  
  # Create the chart based on type
  result <- switch(chart_type,
    "pie" = create_pie_chart(chart_data, chart_options),
    "donut" = create_donut_chart(chart_data, chart_options),
    "bar" = create_bar_chart(chart_data, chart_options),
    "treemap" = create_treemap_chart(chart_data, chart_options),
    stop("Unsupported chart type: ", chart_type)
  )
  
  message("✓ Chart created successfully")
  return(result)
}

#' Prepare Data for Chart Visualization
#' 
#' Processes data for chart visualization, including label merging
#' Based on original app logic (lines 654-657)
#'
#' @param data Raw data from sibdata
#' @param chart_type Type of chart
#' @param r Reactive values object
#' @param con Database connection
#' @return Processed data ready for charting
prepare_chart_data <- function(data, chart_type, r, con) {
  message("=== Preparing chart data ===")
  
  # Data is already processed with sib_merge_ind_label() in main data fetch
  # Just ensure we have the required columns for hgmagic
  
  # Debug: Print data structure
  message("Data columns: ", paste(names(data), collapse = ", "))
  message("Data rows: ", nrow(data))
  if(nrow(data) > 0) {
    message("Sample data:")
    for(i in seq_len(min(3, nrow(data)))) {
      row_data <- sapply(data[i,], function(x) if(is.null(x)) "NULL" else as.character(x))
      message("Row ", i, ": ", paste(names(data), "=", row_data, collapse = " | "))
    }
  }
  
  # Replace indicator slugs with human-friendly labels for legend/axes
  if ("indicador" %in% names(data)) {
    # Safely coerce to character first
    ind_vec <- as.character(data$indicador)
    # Merge labels using DB dictionary
    labeled <- tryCatch({
      sib_merge_ind_label(ind_vec, con = con)
    }, error = function(e) {
      message("⚠️ Could not merge indicator labels: ", e$message)
      ind_vec
    })
    data$indicador <- as.character(labeled)
  }

  # Ensure we have the required columns
  if(!"count" %in% names(data) && "value" %in% names(data)) {
    data$count <- data$value
  }
  
  # hgmagic expects data in specific format - don't modify the structure
  # Just ensure we have the required columns
  required_cols <- c("indicador", "count")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  
  if(length(missing_cols) > 0) {
    message("❌ Warning: Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  message("✓ Data prepared with ", nrow(data), " rows")
  message("Final columns: ", paste(names(data), collapse = ", "))
  
  return(data)
}

#' Get Chart Options
#' 
#' Generates chart options including palette and styling
#' Based on original app logic (lines 734-787)
#'
#' @param chart_type Type of chart
#' @param r Reactive values object
#' @return List of chart options
get_chart_options <- function(chart_type, r) {
  message("=== Getting chart options ===")
  
  # Base options
  opts <- list(
    color_by = 1,
    color_palette_categorical = NULL,
    color_palette_numeric = NULL
  )
  
  # Theme-specific palettes (from original app lines 734-752)
  if(!is.null(r$sel_tematica)) {
    if(grepl("amenazadas", r$sel_tematica)) {
      message("Applying amenazadas palette")
      opts$color_palette_categorical <- c("#d9453d", "#d8783d", "#d7a900")
    } else if(grepl("cites", r$sel_tematica)) {
      message("Applying cites palette")
      opts$color_palette_categorical <- c("#00AFFF", "#000000", "#FFD150", "#4DD3AC")
    }
  }
  
  # Chart-specific options (from original app lines 761-765)
  if(chart_type %in% c("pie", "donut")) {
    opts <- c(opts, list(
      legend_align = "right",
      legend_vertical_align = "middle",
      axis_text_wrap = 100
    ))
  }
  
  message("✓ Chart options configured")
  return(opts)
}

#' Create Pie Chart
#' 
#' Creates a pie chart using hgmagic
#'
#' @param data Prepared chart data
#' @param options Chart options
#' @return hgmagic pie chart
create_pie_chart <- function(data, options) {
  message("Creating pie chart...")
  
  # Prepare arguments for hgmagic
  args <- c(
    list(data = data),
    options
  )
  
  # Remove NULL values
  args <- args[!sapply(args, is.null)]
  
  # Call hgmagic function
  tryCatch({
    result <- do.call(hgmagic::hg_pie_CatNum, args)
    message("✓ Pie chart created")
    return(result)
  }, error = function(e) {
    message("❌ Error creating pie chart: ", e$message)
    return(NULL)
  })
}

#' Create Donut Chart
#' 
#' Creates a donut chart using hgmagic
#'
#' @param data Prepared chart data
#' @param options Chart options
#' @return hgmagic donut chart
create_donut_chart <- function(data, options) {
  message("Creating donut chart...")
  
  # Prepare arguments for hgmagic
  args <- c(
    list(data = data),
    options
  )
  
  # Remove NULL values
  args <- args[!sapply(args, is.null)]
  
  # Call hgmagic function
  tryCatch({
    result <- do.call(hgmagic::hg_donut_CatNum, args)
    message("✓ Donut chart created")
    return(result)
  }, error = function(e) {
    message("❌ Error creating donut chart: ", e$message)
    return(NULL)
  })
}

#' Create Bar Chart
#' 
#' Creates a bar chart using hgmagic
#'
#' @param data Prepared chart data
#' @param options Chart options
#' @return hgmagic bar chart
create_bar_chart <- function(data, options) {
  message("Creating bar chart...")
  
  # Prepare arguments for hgmagic
  args <- c(
    list(data = data),
    options
  )
  
  # Remove NULL values
  args <- args[!sapply(args, is.null)]
  
  # Call hgmagic function
  tryCatch({
    result <- do.call(hgmagic::hg_bar_CatNum, args)
    message("✓ Bar chart created")
    return(result)
  }, error = function(e) {
    message("❌ Error creating bar chart: ", e$message)
    return(NULL)
  })
}

#' Create Treemap Chart
#' 
#' Creates a treemap chart using hgmagic
#'
#' @param data Prepared chart data
#' @param options Chart options
#' @return hgmagic treemap chart
create_treemap_chart <- function(data, options) {
  message("Creating treemap chart...")
  
  # Prepare arguments for hgmagic
  args <- c(
    list(data = data),
    options
  )
  
  # Remove NULL values
  args <- args[!sapply(args, is.null)]
  
  # Call hgmagic function
  tryCatch({
    result <- do.call(hgmagic::hg_treemap_CatNum, args)
    message("✓ Treemap chart created")
    return(result)
  }, error = function(e) {
    message("❌ Error creating treemap chart: ", e$message)
    return(NULL)
  })
}

#' Get Chart Function Name
#' 
#' Maps chart types to their corresponding hgmagic function names
#' Based on original app logic (line 819)
#'
#' @param chart_type Type of chart
#' @return Function name for hgmagic
get_chart_function_name <- function(chart_type) {
  switch(chart_type,
    "pie" = "hgmagic::hg_pie_CatNum",
    "donut" = "hgmagic::hg_donut_CatNum", 
    "bar" = "hgmagic::hg_bar_CatNum",
    "treemap" = "hgmagic::hg_treemap_CatNum",
    "map" = "choropleth_map",
    "table" = NULL,
    stop("Unknown chart type: ", chart_type)
  )
}

#' Validate Chart Data
#' 
#' Validates that data has the required structure for charting
#'
#' @param data Data to validate
#' @param chart_type Type of chart
#' @return TRUE if valid, FALSE otherwise
validate_chart_data <- function(data, chart_type) {
  if(is.null(data) || nrow(data) == 0) {
    message("❌ Data validation failed: empty or null data")
    return(FALSE)
  }
  
  # Check for required columns based on chart type
  if(chart_type %in% c("pie", "donut", "treemap", "bar")) {
    # Need at least one categorical and one numerical column
    has_numeric <- any(sapply(data, is.numeric))
    has_categorical <- any(sapply(data, function(x) is.character(x) || is.factor(x)))
    
    if(!has_numeric) {
      message("❌ Data validation failed: no numeric columns found")
      return(FALSE)
    }
    
    if(!has_categorical) {
      message("❌ Data validation failed: no categorical columns found")
      return(FALSE)
    }
  }
  
  message("✓ Data validation passed")
  return(TRUE)
}