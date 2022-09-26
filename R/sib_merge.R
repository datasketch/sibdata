
sib_region_labels <- function(){
  sibdata_region() |>
    select(slug, label)
}


#' @export
sib_merge_region_label <- function(d, slug = "slug", label = "label_region"){
  if(label %in% colnames(d)){
    warning("Overwritting existing label column: ", label,
            " Use the label param to rename the output label column.")
  }
  regs_label <- sibdata_region() |>
    select(slug_region = slug, label_region = label)

  if(slug == "slug"){
    by <- c("slug" = "slug_region")
  } else if(slug == "slug_region"){
    by <- "slug_region"
  } else {
    stop('slug must be "slug" or "slug_region"')
  }

  if(!slug %in% colnames(d)){
    stop("Region slug column not found")
  }else{
    d2 <- d |>
      left_join(regs_label, by = by, copy = TRUE)
  }
  d2
}


#' @export
sib_merge_grupo_label <- function(d, slug){
  grupo_labels <- sibdata_grupo() |>
    select(slug_grupo = slug, label_grupo = label)

  if(slug == "slug"){
    by <- c("slug" = "slug_grupo")
  } else if(slug == "slug_grupo"){
    by <- "slug_grupo"
  } else {
    stop('slug must be "slug" or "slug_grupo"')
  }

  if(!slug %in% colnames(d)){
    stop("Region slug column not found")
  }else{
    d2 <- d |>
      left_join(grupo_labels, by = by, copy = TRUE)
  }
  d2 |>
    relocate(label_grupo, .after = slug_grupo)

}



#' @export
sib_merge_ind_label <- function(d, replace = TRUE){
  inds <- sib_tables("ind_meta") |>
    #filter(indicador %in% names(d)) |>
    select(indicador,label_ind = label)
  dd <- left_join(d, inds, by = "indicador")
  if(replace){
    dd <- dd |>
      select(-indicador) |>
      select(indicador = label_ind, everything())
  }
  dd

}


#' @export
sib_merge_especie_label <- function(x){
  especie <- sib_tables("especie") |>
    mutate(label = species)
  x |>
    left_join(especie, by = c("slug_especie"="slug")) |>
    select(!contains("slug_region")) |>
    select(label, registros, everything())
}



