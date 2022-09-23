

region_gr_bio_data <- function(region){

  reg_gr_bio <- sib_tables("region_grupo") |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(tipo == "biologico") |>
    dplyr::mutate(slug = slug_grupo) |>
    dplyr::relocate(slug)

  parent <- sib_parent_region(region)
  reg_gr_bio_parent <- sib_tables("region_grupo") |>
    dplyr::filter(slug_region == parent) |>
    dplyr::filter(tipo == "biologico") |>
    dplyr::mutate(slug = slug_grupo) |>
    dplyr::relocate(slug) |>
    left_join(sib_tables("region"), by = c("slug_region" = "slug"))
  reg_gr_bio_parent <- reg_gr_bio_parent |>
    dplyr::select(slug, label, especies_region_total, registros_region_total)




  #keys <- reg_gr_bio |> group_by(slug) |> group_keys()
  reg_gr_bio_list <- reg_gr_bio |>
    group_split(slug)

  i <- 1
  reg_gr_bio_list <- map(reg_gr_bio_list, function(x){
    #message(i)
    i <<- i + 1
    #x <- reg_gr_bio_list[[3]]
    x <- as.list(x)

    x$parent <- reg_gr_bio_parent |>
      filter(slug == x$slug)

    x$estimadas <- estimadas_grupo(x$slug)

    species_list <- list_species(region, grupo = x$slug)
    species_list_top <- species_list |>
      arrange(desc(registros)) |>
      slice(1:10)
    x$species_list_top <- species_list_top

    tematicas <- c("amenazadas-nacional", "amenazadas-global", "cites", "migratorias",
                   "endemicas", "exoticas")
    species_list_tematica <- map(tematicas, function(tem){
      #tem <- tematicas[1]
      #message(tem)
      spe <- list_species(region, grupo = x$slug, tematica = tem)
      spe <- spe |>
        arrange(desc(registros)) |>
        slice(1:10) |>
        select(species, registros, link_gbif = GBIF, link_cbc = CBC)
      spe
    })
    names(species_list_tematica) <- tematicas
    x$species_list_tematica <- species_list_tematica
    x
  })
  reg_gr_bio_list
}

region_gr_int_data <- function(region){
  reg_gr_bio <- sib_tables("region_grupo") |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(tipo == "interes") |>
    dplyr::mutate(slug = slug_grupo) |>
    dplyr::relocate(slug)

  parent <- sib_parent_region(region)
  reg_gr_bio_parent <- sib_tables("region_grupo") |>
    dplyr::filter(slug_region == parent) |>
    dplyr::filter(tipo == "interes") |>
    dplyr::mutate(slug = slug_grupo) |>
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

    x$estimadas <- estimadas_grupo(x$slug)

    species_list <- list_species(region, grupo = x$slug)
    species_list_top <- species_list |>
      arrange(desc(registros)) |>
      slice(1:10)
    x$species_list_top <- species_list_top
    species_list_bottom <- species_list |>
      arrange(registros) |>
      slice(1:10)

    tematicas <- c("amenazadas-nacional", "amenazadas-global", "cites", "migratorias",
                   "endemicas", "exoticas", "exoticas_riesgo_invasion", "invasoras")
    species_list_tematica <- map(tematicas, function(tem){
      #tem <- tematicas[6]
      spe <- list_species(region, grupo = x$slug, tematica = tem)
      spe <- spe |>
        arrange(desc(registros)) |>
        slice(1:10) |>
        select(species, registros)
      spe
    })
    names(species_list_tematica) <- tematicas
    x$species_list_tematica <- species_list_tematica
    x
  })
  reg_gr_bio_list
}
