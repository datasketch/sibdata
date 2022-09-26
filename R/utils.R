
pkg_resources <- function(...){
  system.file(..., package = "sibdata")
}

#' @export
copy_icons <- function(path){
  icons <- pkg_resources("icons")
  dir.create(path)
  file.copy(icons, path, recursive=TRUE)

}

select_non_single_cat_cols <- function(x){
  ind_count <- x |> select(indicador, count)
  has_unique_vals <- function(xx){
    if(is.numeric(xx)) return(FALSE)
    if(all(is.na(xx))) return(FALSE)
    length(unique(xx))== 1
  }
  x |>
    select(-indicador, -count) |>
    select_if(~!has_unique_vals(.)) |>
    bind_cols(ind_count)
}



#' @export
sys_file <- function(...){
  system.file(..., package = "sibdata")
}

`%||%` <- function (x, y){
  suppressWarnings({
    if (is.empty(x))
      return(y)
    else if (is.null(x) || is.na(x))
      return(y)
    else if (class(x) == "character" && all(nchar(x) == 0))
      return(y)
    else x
  })
}

is.empty <- function(x){
  !as.logical(length(x))
}



