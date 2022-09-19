
#' @export
sib_available_tables <- function(){
  sibdata::available_tables
}



#' @export
sib_available_regions <- function(){
  reg_gr <- sib_tables("region_grupo_tematica") |>
    dplyr::distinct(slug_region) |>
    select(slug_region) |>
    sib_merge_region_label()
  av_regs <- reg_gr$slug_region
  names(av_regs) <- reg_gr$label
  av_regs
}

#' @export
sib_parent_region <- function(region){
  sib_tables("region") |>
    dplyr::filter(slug == region) |> dplyr::pull(parent)
}

#' @export
sib_available_subregions <- function(region){
  region <- sib_tables("region") |>
    dplyr::filter(parent == region)
  region |> dplyr::pull(slug)
}

#' @export
sib_available_profile_types <- function(){
  c("region", "territorio", "grupo_biologico", "grupo_interes",
    "specie", "tematica")
}

#' @export
sib_available_grupos <- function(tipo = NULL){

  if(is.null(tipo)){
    gr_bio <- sib_tables("grupo_biologico")
    gr_int <- sib_tables("grupo_interes_conservacion") |>
      mutate(parent = as.character(parent))
    grps <- bind_rows(gr_bio, gr_int) |>
      select(slug_grupo = slug, label)
  } else {
    grps <- sib_tables("region_grupo_tematica") |>
      filter(grupo_tipo == tipo) |>
      select(slug_grupo) |>
      sib_merge_grupo_label()

  }
  av_grps <- grps$slug_grupo
  names(av_grps) <- grps$label
  av_grps

}

#' @export
sib_available_tematicas <- function(){
  available_tematicas <-c(
    "Amenazadas Nacional" ="amenazadas_nacional",
    "Amenazadas Global" ="amenazadas_global",
    "Objeto de comercio (CITES)" = "cites",
    "Endémicas" =  "endemicas",
    "Migratorias" = "migratorias",
    "Exóticas" = "exoticas",
    "Invasoras" = "invasoras",
    "Exóticas riesgo invación" = "exoticas_riesgo_invasion"
  )
  available_tematicas
}


