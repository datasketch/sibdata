
#' #' @export
#' sib_available_tables <- function(){
#'   sibdata::available_tables
#' }



#' @export
sib_available_regions <- function(subtipo = NULL, departamento = NULL){
  regs <- sibdata_region()
  reg_gr <- sibdata_region_grupo() |>
    select(slug_region)
  sel_subtipo <- subtipo
  if(!is.null(subtipo)){
    regs <- regs |>
      filter(subtipo %in% sel_subtipo)
  }
  regs <- regs |> semi_join(reg_gr, by = c("slug" = "slug_region"))

  if(subtipo == "Municipio" & !is.null(departamento)){
    regs <- regs |>
      filter(parent == departamento)
  }

  av_regs <- regs |> pull(slug)
  names(av_regs) <- regs |> pull(label)
  av_regs
}



#' @export
sib_available_subregions <- function(region){
  region <- sibdata_region() |>
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


