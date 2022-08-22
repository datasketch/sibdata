
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


