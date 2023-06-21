
#' @export
region_indicadores <- function(region, indicadores, con = NULL){
  sib_validate_indicadores(indicadores, con)
  d <- region_tematica(region, con) |> collect()
  if(!all(indicadores %in% colnames(d))){
    stop("Not all indicadores in region_tematica table: ",
         mop::which_not_in(indicadores, names(d)))
  }
  d <- d |>
    select(any_of(indicadores))
  l <- as.list(d)
  if(!is.null(names(indicadores))){
    names(l) <- names(indicadores)
  }
  l
}


#' @export
default_indicadores <- function(section){

  if(section == "inds_amenazadas_nacional"){
    inds <- c(
      "especies" = "especies_amenazadas_nacional_total",
      "registros" = "registros_amenazadas_nacional_total",
      "cr" = "especies_amenazadas_nacional_cr",
      "cr_registros" = "registros_amenazadas_nacional_cr",
      "en" = "especies_amenazadas_nacional_en",
      "en_registros" = "registros_amenazadas_nacional_en",
      "vu" = "especies_amenazadas_nacional_vu",
      "vu_registros" = "registros_amenazadas_nacional_vu",
      "especies_estimadas" = "especies_region_estimadas"
    )
  }

  if(section == "parent_inds_amenazadas_nacional"){
    inds <- c(
      "parent_especies_est" = "especies_region_estimadas",
      "parent_cr" = "especies_amenazadas_nacional_cr",
      "parent_en" = "especies_amenazadas_nacional_en",
      "parent_vu" = "especies_amenazadas_nacional_vu"
    )
  }


  if(section == "inds_amenazadas_global"){
    inds <- c(
      "especies" = "especies_amenazadas_global_total",
      "registros" = "registros_amenazadas_global_total",
      "cr" = "especies_amenazadas_global_cr",
      "cr_registros" = "registros_amenazadas_global_cr",
      "en" = "especies_amenazadas_global_en",
      "en_registros" = "registros_amenazadas_global_en",
      "vu" = "especies_amenazadas_global_vu",
      "vu_registros" = "registros_amenazadas_global_vu",
      "especies_estimadas" = "especies_region_estimadas"
    )
  }

  if(section == "parent_inds_amenazadas_global"){
    inds <- c(
      "parent_especies_est" = "especies_region_estimadas",
      "parent_cr" = "especies_amenazadas_global_cr",
      "parent_en" = "especies_amenazadas_global_en",
      "parent_vu" = "especies_amenazadas_global_vu"
    )
  }

  if(section == "inds_especies_est"){
    inds <- c(
      "especies_total" = "especies_region_total"
    )
  }

  if(section == "inds_especies_parent_total"){
    inds <- c(
      "parent_especies_total" = "especies_region_total"
    )
  }

  if(section == "inds_cites"){
    inds <- c(
      "especies_cites_total" = "especies_cites_total",
      "especies_cites_i" = "especies_cites_i",
      "especies_cites_ii" = "especies_cites_ii",
      "especies_cites_i_ii" = "especies_cites_i_ii",
      "especies_cites_iii" = "especies_cites_iii",
      "registros_cites_total" = "registros_cites_total",
      "registros_cites_i" = "registros_cites_i",
      "registros_cites_ii" = "registros_cites_ii",
      "registros_cites_i_ii" = "registros_cites_i_ii",
      "registros_cites_iii" = "registros_cites_iii"
    )
  }
  if(section == "inds_parent_cites"){
    inds <- c(
      "parent_especies_cites_total" = "especies_cites_total",
      "parent_especies_cites_i" = "especies_cites_i",
      "parent_especies_cites_ii" = "especies_cites_ii",
      "parent_especies_cites_i_ii" = "especies_cites_i_ii",
      "parent_especies_cites_iii" = "especies_cites_iii",
      "parent_registros_cites_total" = "registros_cites_total",
      "parent_registros_cites_i" = "registros_cites_i",
      "parent_registros_cites_ii" = "registros_cites_ii",
      "parent_registros_cites_i_ii" = "registros_cites_i_ii",
      "parent_registros_cites_iii" = "registros_cites_iii"
    )
  }

  if(section == "inds_endemicas"){
    inds <- c(
      "especies_endemicas" = "especies_endemicas",
      "registros_endemicas" = "registros_endemicas"
    )
  }
  if(section == "inds_parent_endemicas"){
    inds <- c(
      "parent_especies_endemicas" = "especies_endemicas",
      "parent_registros_endemicas" = "registros_endemicas"
    )
  }
  if(section == "inds_migratorias"){
    inds <- c(
      "especies_migratorias" = "especies_migratorias",
      "registros_migratorias" = "registros_migratorias"
    )
  }
  if(section == "inds_parent_migratorias"){
    inds <- c(
      "parent_especies_migratorias" = "especies_migratorias",
      "parent_registros_migratorias" = "registros_migratorias"
    )
  }

  if(section == "inds_exoticas"){
    inds <- c(
      "especies_exoticas_total" = "especies_exoticas_total",
      "especies_exoticas" = "especies_exoticas",
      "especies_invasoras" = "especies_invasoras",
      "especies_exoticas_riesgo_invasion" = "especies_exoticas_riesgo_invasion",
      "registros_exoticas_total" = "registros_exoticas_total",
      "registros_exoticas" = "registros_exoticas",
      "registros_invasoras" = "registros_invasoras",
      "registros_exoticas_riesgo_invasion" = "registros_exoticas_riesgo_invasion"
    )
  }
  if(section == "inds_parent_exoticas"){
    inds <- c(
      "parent_especies_exoticas_total" = "especies_exoticas_total",
      "parent_especies_exoticas" = "especies_exoticas",
      "parent_especies_invasoras" = "especies_invasoras",
      "parent_especies_exoticas_riesgo_invasion" = "especies_exoticas_riesgo_invasion",
      "parent_registros_exoticas_total" = "registros_exoticas_total",
      "parent_registros_exoticas" = "registros_exoticas",
      "parent_registros_invasoras" = "registros_invasoras",
      "parent_registros_exoticas_riesgo_invasion" = "registros_exoticas_riesgo_invasion"
    )
  }

  inds

}



