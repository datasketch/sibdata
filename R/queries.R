

tematica_list <- function(region){

  reg_tematica <- region_tematica(region)

  esp_label <- sib_tables("especie") |> select(slug, species)

  esp_reg <- sib_tables("especie_region") |>
    dplyr::filter(slug_region == region) |>
    left_join(esp_label, by = c("slug_especie" = "slug"))


  esp_tem <- sib_tables("especie_tematica")
  esp_reg_tem <-  esp_reg |>
    dplyr::left_join(esp_tem) |>
    dplyr::select(-registros) |>
    distinct()


  tem <- sib_tables("tematica") #|>
  #dplyr::filter(is.na(orden))

  d2 <- reg_tematica |>
    dplyr::select(-fecha_corte) |>
    tidyr::pivot_longer(-starts_with("slug"),
                        names_to = c("indicador"),
                        values_to = "count")

  inds <- sib_tables("ind_meta") #|>
  #filter(indicador %in% names(d))
  d3 <- dplyr::left_join(d2, inds)
  d4 <- d3 |>
    dplyr::select_if(~length(unique(.))!= 1) |>
    dplyr::select(-indicador)
  d5 <- d4 %>% dplyr::relocate(count, .after = last_col())
  d6 <- d5 |>
    dplyr::filter(cobertura == "total") |>
    dplyr::filter(tipo == "especies") |>
    dplyr::filter(!is.na(slug_tematica)) |>
    dplyr::select(slug_tematica, label, count)

  tems_list <- d6 |> dplyr::group_split(slug_tematica)
  tem_groups <- d6 |> dplyr::group_by(slug_tematica) |>
    dplyr::group_keys() |> dplyr::pull(slug_tematica)
  #names(tems_list) <- tem_groups


  tematica_list <- purrr::map(tems_list, function(x){
    #x <- tems_list[[17]]
    x <- as.list(x)
    x$slug <- x$slug_tematica
    esps_tem <- esp_reg_tem |>
      dplyr::select(-slug_region) |>
      dplyr::filter(grepl(x$slug, slug_tematica)) |>
      left_join(esp_reg)

    esps <- esps_tem |>
      select(species, registros) |>
      distinct() |>
      arrange(desc(registros)) |>
      slice(1:x$count)

    #distinct(slug_especie, .keep_all = TRUE)
    #x$especies <- list(esps_tem)
    x$title <- paste0("Lista especies: ", x$slug)
    #chart <- sib_chart_gt_table2(esps_tem)
    slug <- x$slug
    #path <- glue::glue("static/charts/{region}/{slug}.html")
    #gt::gtsave(chart, path)
    #x$chart <- path
    x$species_list <- esps
    x
  })


  amenazadas_global <- tematica_list[[1]][1:3]
  amenazadas_nacional <- tematica_list[[12]][1:3]
  endemicas <- tematica_list[[21]][1:3]
  migratorias <- tematica_list[[25]][1:3]

  tematica_list0 <- list(
    list(
      slug = "amenazadas",
      label = "Las especies amenazadas se dividen en Nacional y Global",
      children = list(amenazadas_global, amenazadas_nacional)
    ),
    list(
      slug = "distribución",
      label = "Las especies amenazadas se dividen en Nacional y Global",
      children = list(endemicas, migratorias)
    )
  )



  c(tematica_list0, tematica_list)

}






region_gr_bio_data <- function(region){
  reg_gr_bio <- sib_tables("region_grupo_biologico") |>
    dplyr::filter(slug_region == region) |>
    dplyr::mutate(slug = slug_grupo_biologico) |>
    dplyr::relocate(slug)

  parent <- sib_parent_region(region)
  reg_gr_bio_parent <- sib_tables("region_grupo_biologico") |>
    dplyr::filter(slug_region == parent) |>
    dplyr::mutate(slug = slug_grupo_biologico) |>
    dplyr::relocate(slug) |>
    left_join(sib_tables("region"), by = c("slug_region" = "slug"))
  reg_gr_bio_parent <- reg_gr_bio_parent |>
    dplyr::select(slug, label, especies_region_total, registros_region_total)


  #keys <- reg_gr_bio |> group_by(slug) |> group_keys()
  reg_gr_bio_list <- reg_gr_bio |>
    group_split(slug)
  reg_gr_bio_list <- map(reg_gr_bio_list, function(x){
    #x <- reg_gr_bio_list[[1]]
    x <- as.list(x)

    x$parent <- reg_gr_bio_parent |>
      filter(slug == x$slug)

    species_list <- list_species(region, grupo_biologico = x$slug)
    species_list_top <- species_list |>
      arrange(desc(registros)) |>
      slice(1:50)
    x$species_list_top <- species_list_top
    species_list_bottom <- species_list |>
      arrange(registros) |>
      slice(1:500)

    tematicas <- c("amenazadas-nacional", "amenazadas-global", "cites", "migratorias",
                   "endemicas", "exoticas", "exoticas_riesgo_invasion", "invasoras")
    species_list_tematica <- map(tematicas, function(tem){
      #tem <- tematicas[6]
      spe <- list_species(region, grupo_biologico = x$slug, tematica = tem)
      spe <- spe |>
        arrange(desc(registros)) |>
        slice(1:500) |>
        select(species, registros)
      spe
    })
    names(species_list_tematica) <- tematicas
    x$species_list_tematica <- species_list_tematica
    x
  })
  reg_gr_bio_list
}

region_gr_int_data <- function(region){
  reg_gr_int <- sib_tables("region_grupo_interes_conservacion") |>
    dplyr::filter(slug_region == region) |>
    dplyr::mutate(slug = slug_grupo_interes_conservacion) |>
    dplyr::relocate(slug)

  parent <- sib_parent_region(region)
  reg_gr_int_parent <- sib_tables("region_grupo_interes_conservacion") |>
    dplyr::filter(slug_region == parent) |>
    dplyr::mutate(slug = slug_grupo_interes_conservacion) |>
    dplyr::relocate(slug) |>
    left_join(sib_tables("region"), by = c("slug_region" = "slug"))
  reg_gr_int_parent <- reg_gr_int_parent |>
    dplyr::select(slug, label, especies_region_total, registros_region_total)


  reg_gr_int_list <- reg_gr_int |>
    group_split(slug)
  reg_gr_int_list <- map(reg_gr_int_list, function(x){
    #x <- reg_gr_bio_list[[1]]
    x <- as.list(x)

    x$parent <- reg_gr_int_parent |>
      filter(slug == x$slug)

    species_list <- list_species(region, grupo_interes = x$slug)
    species_list_top <- species_list |>
      arrange(desc(registros)) |>
      slice(1:500)
    x$species_list_top <- species_list_top
    species_list_bottom <- species_list |>
      arrange(registros) |>
      slice(1:500)
    x$species_list_bottom <- species_list_bottom

    tematicas <- c("amenazadas-nacional", "amenazadas-global", "cites", "migratorias",
                   "endemicas", "exoticas", "exoticas_riesgo_invasion", "invasoras")
    species_list_tematica <- map(tematicas, function(tem){
      #tem <- tematicas[6]
      spe <- list_species(region, grupo_interes = x$slug, tematica = tem)
      spe <- spe |>
        arrange(desc(registros)) |>
        slice(1:500) |>
        select(species, registros)
      spe
    })
    names(species_list_tematica) <- tematicas
    x$species_list_tematica <- species_list_tematica


    x
  })
  reg_gr_int_list
}

region_tematica <- function(region){
  reg_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == region)
  reg_tematica
}


region_grupo_tematica <- function(region, grupo){
  reg_grp_tematica <- sib_tables("region_grupo_tematica") |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(slug_grupo == grupo)
  reg_grp_tematica
}

subregion_tematica <- function(region){

  regs <- ds$region |> select(slug_region = slug, label)

  subregs <- sib_available_subregions(region)
  subreg_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region %in% subregs) |>
    dplyr::left_join(regs, by = "slug_region") |>
    dplyr::select(-fecha_corte) |>
    dplyr::relocate(slug_region, label, everything())
  subreg_tematica
}

parent_tematica <- function(region){
  parent <- sib_parent_region(region)
  parent_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == parent)
  parent_tematica
}

with_parent_tematica <- function(region){
  parent <- sib_parent_region(region)
  with_parent_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region %in% c(region, parent))
  with_parent_tematica
}



