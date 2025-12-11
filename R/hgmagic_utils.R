hc_add_bar <- function(hc, data, hdtype, ...) {
  opts <- c(
    dsopts_merge(..., categories = "bar"),
    dsopts_merge(..., categories = "axis")
  )

  opts_theme <-  dsopts_merge(..., categories = "colorprep")
  opts_theme$palette_colors <- opts_theme$color_palette_categorical
  bar_type <- if (opts$bar_orientation == "ver") "column" else "bar"

  hc <- hc |>
    hc_chart(
      type = bar_type
    )

  if (opts$bar_orientation == "hor") {
    title_axis_x <- opts$title_axis_y
    title_axis_y <- opts$title_axis_x
    opts$title_axis_x <- title_axis_x
    opts$title_axis_y <- title_axis_y
  }

  # Handle different hdtype scenarios with consolidated conditional logic
  if (hdtype == "CatNum") {
    opts$legend_show <- FALSE
    opts$bar_graph_type <- "grouped"
    hc <- hc |> add_CatNum_features(data, opts, bar_type)
  }

  hc <- hc |> hc_colors(opts_theme$palette_colors) |>
    hc_legend(enabled = FALSE)

  hc
}

hc_add_pie <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "pie"),
            dsopts_merge(..., categories = "legend"))

  hc <- hc |>
    hc_chart(type = "pie") |>
    add_CatNum_features(data, opts, "pie")
  opts_theme <-  dsopts_merge(..., categories = "colorprep")
  opts_theme$palette_colors <- opts_theme$color_palette_categorical
  hc <- hc |> hc_colors(opts_theme$palette_colors) |>
    hc_plotOptions(
      series = list(
        dataLabels = list(enabled = FALSE)
      ),
      pie = list(
        showInLegend = TRUE)
    )

  hc

}

hc_add_donut <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "donut"),
            dsopts_merge(..., categories = "legend"))
  hc <- hc |>
    hc_chart(type = "pie") |>
    add_CatNum_features(data, opts, "pie") |>
    hc_plotOptions(
      series = list(
        dataLabels = list(enabled = FALSE)
      ),
      pie = list(
      showInLegend = TRUE,
      innerSize = 180))
  opts_theme <-  dsopts_merge(..., categories = "colorprep")
  opts_theme$palette_colors <- opts_theme$color_palette_categorical
  hc <- hc |> hc_colors(opts_theme$palette_colors)

  hc

}


hc_add_treemap <- function(hc, data, hdtype, ...) {

  opts <- c(
    dsopts_merge(..., categories = "treemap"),
    dsopts_merge(..., categories = "legend")
  )

  opts_color <- dsopts_merge(..., categories = "colorprep")

  hc <- hc |>
    hc_chart(type = "treemap")

  colors <- opts_color$color_palette_categorical

  # Handle different hdtype scenarios with consolidated conditional logic
  if (hdtype == "CatNum") {
    hc <- hc |> add_CatNum_features(data, opts, "treemap")
  }
  hc <- hc |> hc_colors(colors) |>
    hc_plotOptions(
    series = list(
      colorByPoint = TRUE
    ))
  hc

}





add_CatNum_features <- function(hc, data, opts, viz) {

  if (viz %in% c("treemap", "bubble")) {
    hc <- hc |>
      hc_data_series(data)
  } else {
    hc <- hc |>
      hc_data_series(data$data)
  }

  if (viz %in% c("bar", "column")) {
    hc <- hc |>
      hc_axis(
        axis = "x", categories = data$categories,
        type = "category", opts = opts
      ) |>
      hc_axis(axis = "y", opts = opts)
  }

  hc
}




hc_axis <- function(hc, axis = "x", categories = NULL, type = NULL, opts, double_axis = FALSE) {
  if (!axis %in% c("x", "y")) {
    stop("axis must be 'x' or 'y'")
  }

  # Handle double axis functionality
  if (double_axis && axis == "y") {
    return(hc_axis_double_y(hc, opts))
  }


  axis_function <- if (axis == "x") hc_xAxis else hc_yAxis
  axis_title <- if (axis == "x") opts$title_axis_x else opts$title_axis_y
  axis_labels <- NULL
  axis_format <- NULL
  if (axis == "y") {
    axis_labels <-  paste0(opts$axis_y_prefix, "{text}", opts$axis_y_suffix)
    if (!is.null(opts$axis_y_format_sample_num) || !is.null(opts$format_sample_num)) {
      axis_format <- makeup::makeup_format_js(opts$axis_y_format_sample_num, opts$locale,
                                              opts$axis_y_suffix, opts$axis_y_prefix,
                                              opts$use_si_prefixes)
    }
  }

  hc |>
    axis_function(
      categories = categories,
      crossing = NULL,
      endOnTick = FALSE,
      startOnTick = TRUE,
      labels = list(
        format = axis_labels,
        formatter = axis_format
      ),
      title = list(
        style = list(
          color = "#666666",
          fontSize = "0.8em"
        ),
        text = axis_title,
        # textAlign = NULL,
        useHTML = TRUE#,
        # x = 0,
        # y = 0
      )
    )

}


hdtype_viz <- function(var_cat = NULL, var_num = NULL,
                       var_dat = NULL, var_yea = NULL,
                       var_img = NULL) {

  parts <- list()

  if (!is.null(var_cat)) parts <- c(parts, rep("Cat", length(var_cat)))
  if (!is.null(var_yea)) parts <- c(parts, rep("Yea", length(var_yea)))
  if (!is.null(var_dat)) parts <- c(parts, rep("Dat", length(var_dat)))
  if (!is.null(var_img)) parts <- c(parts, rep("Img", length(var_img)))
  if (!is.null(var_num)) parts <- c(parts, rep("Num", length(var_num)))

  hdtype <- paste(parts, collapse = "")
  hdtype
}

data_vars <- function(data) {
  data_names <- names(data)
  data_names
}


#' @export
hc_data_series <- function(hc, data) {

  is_list_of_series <- is.list(data)
  list_in_list <- all(sapply(data, function(x) is.list(x) && !is.null(x$data)))

  # Si los datos son una lista de series, utiliza hc_add_series_list o hc_series dependiendo de la estructura
  if (is_list_of_series) {
    if (list_in_list) {
      return(hc_add_series_list(hc, data))
    } else {
      # Si es una única serie pero aún así es una lista, utiliza hc_series
      return(hc_series(hc, list(data = data)))
    }
  } else {
    # Para un único conjunto de datos que no está dentro de una lista de series
    return(hc_add_series(hc, data))
  }
}






#' @keywords internal
hg_list <- function(data, hdtype, viz = NULL) {

  if (is.null(viz) | is.null(hdtype)) return()

  if (hdtype %in% c("CatNum")) {
    return(process_CatNum(data, viz))
  }


  if (hdtype %in% c("CatCatNum")) {
    return(process_CatCatNum(data, viz))
  }


}

#' Data processing for visualization
#'
#' This set of functions provides tools to process data for visualisation in different types of charts.
#'
#' @param d A data frame containing the data to process. The structure of the data frame varies depending on the processing function.
#' @param viz The desired type of visualisation. It can be "bar", "column", "pie", or "donut".
#' @return A list with the processed data for visualisation.
#' @examples
#' # Example usage of data processing functions
#' d <- data.frame(
#'   category = c("A", "A", "B", "B"),
#'   value = c(10, 20, 30, 40)
#' )
#' process_CatNum(d, "bar")
#' @export

#' @rdname process_functions
process_CatNum <- function(d, viz) {

  if (viz %in% c("bar", "column", "radial_bar", "pie", "donut")) {
    data <- purrr::pmap(
      list(d[[1]], d[[2]]),
      function(name, y) {
        list(
          "name" = as.character(name),
          "y" = as.numeric(y)
        )
      }
    )

    data <- list(
      data = data,
      categories = purrr::map(as.character(d[[1]][!duplicated(d[[1]])]), function(z) z)
    )
  }



  if (viz == "treemap") {
    data <- purrr::pmap(
      list(d[[1]], d[[2]]),
      function(name, value) {
        list(
          "name" = name,
          "value" = value
        )
      }
    )
  }




  data

}



