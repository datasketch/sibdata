
pkg_resources <- function(...){
  system.file(..., package = "sibdata")
}

#' @export
copy_icons <- function(path){
  icons <- pkg_resources("icons")
  dir.create(path)
  file.copy(icons, path, recursive=TRUE)

}

