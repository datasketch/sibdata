
pkg_resources <- function(...){
  system.file(..., package = "sibdata")
}

#' @export
copy_icons <- function(path){
  icons <- pkg_resources("icons")
  dir.create(path)
  file.copy(icons, path, recursive=TRUE)

}


#' @export
sib_merge_region_label <- function(d){
  regs <- sib_tables("region") |> select(slug_region = slug, label)
  if("slug_region" %in% names(d)){
    d <- d |>
      left_join(regs, by = "slug_region") |>
      relocate(slug_region, label, everything())
  }
  if("slug" %in% names(d) && !"slug_region" %in% names(d)){
    d <- d |>
      left_join(regs, by = c("slug"="slug_region")) |>
      relocate(slug, label, everything())
  }
  d
}

