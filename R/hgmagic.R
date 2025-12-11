#' @export
hg_pie <- function(data,
                   dic = NULL,
                   var_cat = NULL,
                   var_yea = NULL,
                   var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num %||% 'count')
  var_cat <- c(var_cat, var_yea)
  data_viz <- hg_list(data, hdtype, "pie")
  highchart() |>
    hc_add_pie(data_viz, hdtype, ...) #|>
  #hc_add_exporting(...)
}

#' @export
hg_pie_CatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_pie(data, dic, var_cat = vars[1], var_num = vars[2], ...)
}



#' @export
hg_bar <- function(data,
                   dic = NULL,
                   var_cat = NULL,
                   var_yea = NULL,
                   var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num)

  data_viz <- hg_list(data, hdtype, "bar")

  h <- highchart() |>
    hc_add_bar(data_viz, hdtype, ...)

  h

}


#' @export
hg_bar_CatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_bar(data, dic, var_cat = vars[1], var_num = vars[2], ...)
}



#' @export
hg_treemap <- function(data,
                       dic = NULL,
                       var_cat = NULL,
                       var_yea = NULL,
                       var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num )


  var_cat <- c(var_cat, var_yea)

  data_viz <- hg_list(data, hdtype, "treemap")

  highchart() |>
    hc_add_treemap(data_viz, hdtype, ...)
}




#' @export
hg_treemap_CatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_treemap(data, dic, var_cat = vars[1], var_num = vars[2], ...)
}



#' @export
hg_donut <- function(data,
                     dic = NULL,
                     var_cat = NULL,
                     var_yea = NULL,
                     var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num %||% 'count')

  data_viz <- hg_list(data, hdtype, "donut")

  highchart() |>
    hc_add_donut(data_viz, hdtype, ...)
}


#' @export
hg_donut_CatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_donut(data, dic, var_cat = vars[1], var_num = vars[2], ...)
}

