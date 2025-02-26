
#' #' @export
#' sib_available_tables <- function(){
#'   sibdata::available_tables
#' }



#' @export
sib_available_regions <- function(subtipo = NULL, departamento = NULL, con = con){
  regs <- sibdata_region(con) |> collect()
  reg_gr <- sibdata_region_grupo(con) |>
    #select(slug_region) |>
    collect()
  sel_subtipo <- subtipo
  if(!is.null(subtipo)){
    regs <- regs |>
      filter(subtipo %in% sel_subtipo)
  }
  #regs <- regs |> semi_join(reg_gr, by = c("slug" = "slug_region"))

  if(subtipo == "Municipio" & !is.null(departamento)){
    regs <- regs |>
      filter(parent == departamento)
  }

  av_regs <- regs |> pull(slug)
  names(av_regs) <- regs |> pull(label)
  av_regs
}



#' @export
sib_available_subregions <- function(region, con){
  region <- sibdata_region(con) |>
    dplyr::filter(parent == region)
  region |> dplyr::pull(slug)
}

#' @export
sib_available_profile_types <- function(){
  c("region", "territorio", "grupo_biologico", "grupo_interes",
    "specie", "tematica")
}

#' @export
sib_available_grupos <- function(tipo = NULL, con){
  grupo_tipo <- tipo
  grupo <- sibdata_grupo(con) |> collect()
  if(!is.null(tipo)){
    grupo <- grupo |>
      filter(tipo == grupo_tipo)
  }
  av_grps <- grupo$slug
  names(av_grps) <- grupo$label
  av_grps

}

#' @export
sib_available_tematicas <- function(){
  available_tematicas <-c(
    #"Amenazadas" = "amenazadas",
    "Amenazadas Nacional" ="amenazadas_nacional",
    "Amenazadas Global" ="amenazadas_global",
    "Objeto de comercio (CITES)" = "cites",
    "Objeto de comercio (CITES I)" = "cites_i",
    "Objeto de comercio (CITES I_II)" = "cites_i_ii",
    "Objeto de comercio (CITES II)" = "cites_ii",
    "Objeto de comercio (CITES III)" = "cites_iii",
    "Endémicas" =  "endemicas",
    "Migratorias" = "migratorias",
    "Exóticas Total" = "exoticas_total",
    "Exóticas" = "exoticas",
    "Invasoras" = "invasoras",
    "Exóticas riesgo invasión" = "exoticas_riesgo_invasion"
  )
  available_tematicas
}


