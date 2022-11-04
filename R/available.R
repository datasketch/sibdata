
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
  grupo_tipo <- tipo
  grupo <- sibdata_grupo() |> collect()
  if(!is.null(tipo)){
    grps <- grupo |>
      filter(tipo == tipo)
  }
  av_grps <- grupo$slug
  names(av_grps) <- grupo$label
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


