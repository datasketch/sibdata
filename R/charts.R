

sib_chart_reg_municipios <- function(d, var = "especies_region_total"){

  x <- d |> dplyr::select(one_of(c("slug_region", var)))
  h <- hgchmagic::hgch_bar_CatNum(x, hor_title = " ", ver_title = " ")
  h
}


#' @export
sib_chart_waffle <- function(d, path){

  x <- d$registros_region_total
  names(x) <- d$slug_region
  x[1] <- x[1] - x[2]
  x <- rev(x)
  x <- round(x/sum(x)*100)

  gg <- waffle::waffle(x, colors = c("orange", "lightgrey"), row = 10,
               flip = TRUE) +
    theme(legend.position = "none")
  gg
}

#' @export
sib_chart_gt_table <- function(t, labels = NULL){
  names(t) <- c("category", "n")
    gt <- gt::gt(t) |>
      #opt_table_font("Space Grotesk") |>
      gtExtras::gt_plt_bar(column = n, keep_column = TRUE,
                     width = 35, color = "#3e55ff") |>

      gt::tab_style(
        style = gt::cell_text(size = gt::px(12)),
        locations = list(gt::cells_body(), gt::cells_column_labels())
      )
    if(!is.null(labels)){
      labels <- paste0("**", labels, "**")
      gt <- gt |>
        gt::cols_label(category = gt::md(labels[1]), n = gt::md(labels[2]),
                   DUPE_COLUMN_PLT = "")
    }
    gt
}


#' @export
sib_chart_gt_table2 <- function(t, labels = NULL){
  names(t) <- c("especie", "tematica")
  gt <- gt::gt(t) |>
    #opt_table_font("Space Grotesk") |>
    gt::tab_style(
      style = gt::cell_text(size = gt::px(12)),
      locations = list(gt::cells_body(), gt::cells_column_labels())
    )
  gt
}


