
#' @export
sib_merge_region_label <- function(d){
  if(!"label" %in% names(d)){
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
  }
  d
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
sib_merge_grupo_label <- function(d){
  gr_bio <- sib_tables("grupo_biologico")
  gr_int <- sib_tables("grupo_interes_conservacion") |>
    mutate(parent = as.character(parent))
  grps <- bind_rows(gr_bio, gr_int) |>
    select(slug_grupo = slug, label)

  if("slug_grupo" %in% names(d)){
    d <- d |>
      left_join(grps, by = "slug_grupo") |>
      relocate(slug_grupo, label, everything())
  }
  if("slug" %in% names(d) && !"slug_grupo" %in% names(d)){
    d <- d |>
      left_join(grps, by = c("slug"="slug_grupo")) |>
      relocate(slug, label, everything())
  }
  d

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



