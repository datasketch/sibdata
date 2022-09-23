

tematica_list <- function(region){

  # region <- "boyaca"
  #reg_tematica <- region_tematica(region)

  # esp_label <- especie_label()
  # esp_reg <- especie_region(region)
  # esp_reg_tem <- especie_region_tematica(region)
  # tem <- sib_tables("tematica") #|>
  #region_tem_long <- region_tematica_long(region)

  parent_region <- sib_parent_region(region)

  inds_amenazadas_nacional <- default_indicadores("inds_amenazadas_nacional")
  inds_amenazadas_global <- default_indicadores("inds_amenazadas_global")
  inds_especies_parent <- default_indicadores("inds_especies_parent")

  # Amenazadas
  amenazadas <- list(
      slug = "amenazadas",
      label = "Amenazadas",
      children = list(
        c(list("slug" = "amenazadas-nacional", "label" = "Amenazadas nacional"),
          region_indicadores(region, inds_amenazadas_nacional)),
        c(list("slug" = "amenazadas-global", "label" = "Amenazadas global"),
        region_indicadores(region, inds_amenazadas_global))
      ),
      especies_list = NULL
  )

  ## Amenazadas - Amenazadas Nacional
  amenazadas_nacional <- c(
    list(slug = "amenazadas-nacional", label = "Amenazadas nacional"),
    region_indicadores(region, inds_amenazadas_nacional),
    region_indicadores(parent_region, inds_especies_parent),
    list(list_especies_amenazadas_nacional = NULL,
         list_especies_amenazadas_nacional_vu = NULL,
         list_especies_amenazadas_nacional_en = NULL,
         list_especies_amenazadas_nacional_cr = NULL)
  )

  ## Amenazadas - Amenazadas Global
  amenazadas_global <- c(
    list(slug = "amenazadas-global", label = "Amenazadas global"),
    region_indicadores(region, inds_amenazadas_global),
    region_indicadores(region, inds_especies_parent),
    list(list_especies_amenazadas_global = NULL,
         list_especies_amenazadas_global_vu = NULL,
         list_especies_amenazadas_global_en = NULL,
         list_especies_amenazadas_global_cr = NULL)
  )

  # CITES
  inds_cites <- default_indicadores("inds_cites")
  inds_parent_cites <- default_indicadores("inds_parent_cites")
  cites <- c(
    list(slug = "cites"),
    region_indicadores(region, inds_cites),
    region_indicadores(parent_region, inds_parent_cites),
    list(list_especies_cites = NULL,
         list_especies_cites_i = NULL,
         list_especies_cites_ii = NULL,
         list_especies_cites_i_ii = NULL,
         list_especies_cites_iii = NULL)
  )

  # Endémicas
  inds_endemicas <- default_indicadores("inds_endemicas")
  inds_parent_endemicas <- default_indicadores("inds_parent_endemicas")
  endemicas <- c(
    list(slug = "endemicas"),
    region_indicadores(region, inds_endemicas),
    region_indicadores(parent_region, inds_parent_endemicas),
    list(list_especies_endemicas = NULL)
  )


  # Migratorias
  inds_migratorias <- default_indicadores("inds_migratorias")
  inds_parent_migratorias <- default_indicadores("inds_parent_migratorias")
  migratorias <- c(
    list(slug = "migratorias",
         texto = "Existen otras especies migratorias pero aún no están documentadas
         a través del SiB Colombia,"),
    region_indicadores(region, inds_migratorias),
    region_indicadores(parent_region, inds_parent_migratorias),
    list(list_especies_migratorias = NULL)
  )


  # Exóticas
  inds_exoticas <- default_indicadores("inds_exoticas")
  inds_parent_exoticas <- default_indicadores("inds_parent_exoticas")
  exoticas <- c(
    list(slug = "exoticas-invasoras"),
    region_indicadores(region, inds_exoticas),
    region_indicadores(parent_region, inds_especies_parent),
    list(list_especies_exoticas_total = NULL,
         list_especies_exoticas = NULL,
         list_especies_invasoras = NULL,
         list_especies_exoticas_riesgo_invasion = NULL)
  )

  list(
    amenazadas,
    amenazadas_nacional,
    amenazadas_global,
    cites,
    endemicas,
    migratorias,
    exoticas
  )

}




region_tematica <- function(region){
  reg_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == region)
  reg_tematica
}


region_grupo <- function(region, grupo){
  reg_grp_tematica <- sib_tables("region_grupo") |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(slug_grupo == grupo)
  reg_grp_tematica
}




