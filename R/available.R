
#' @export
sib_available_tables <- function(){
  sibdata::available_tables
}



#' @export
sib_available_regions <- function(level = c(1,2)){
  regs <- sib_tables("region")
  if(1 %in% level)
    vars <- "País"
  if(2 %in% level)
    vars <- c(vars, "Departamento")
  regs2 <- regs |>
    filter(subtipo %in% vars)
  regs_level <- regs2$slug

  reg_gr <- sib_tables("region_grupo") |>
    dplyr::distinct(slug_region, .keep_all = TRUE) |>
    select(slug_region) |>
    sib_merge_region_label() |>
    filter(slug_region %in% regs_level)

  av_regs <- reg_gr$slug_region
  names(av_regs) <- reg_gr$label
  av_regs
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
    grps <- sib_tables("region_grupo") |>
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


