

region_grupo_data <- function(region, tipo = "biologico", verbose = FALSE, con){

  sel_tipo <- tipo
  reg_gr_bio <- sibdata_region_grupo(con) |>
    dplyr::filter(slug_region == region) |>
    dplyr::filter(tipo == sel_tipo) |>
    dplyr::mutate(slug = slug_grupo) |>
    dplyr::relocate(slug) |>
    collect() |>
    distinct(slug, .keep_all = TRUE)

  parent <- sib_parent_region(region, con)
  reg_gr_bio_parent <- sibdata_region_grupo(con) |>
    dplyr::filter(slug_region == parent) |>
    dplyr::filter(tipo == sel_tipo) |>
    dplyr::mutate(slug = slug_grupo) |>
    dplyr::relocate(slug) |>
    left_join(sibdata_region(con), by = c("slug_region" = "slug"))
  reg_gr_bio_parent <- reg_gr_bio_parent |>
    dplyr::select(slug, label, especies_region_total, registros_region_total)



  #keys <- reg_gr_bio |> group_by(slug) |> group_keys()
  reg_gr_bio_list <- reg_gr_bio |>
    group_split(slug)
  if(verbose){
    message("Grupos totales: ", nrow(reg_gr_bio))
  }

  i <- 1
  reg_grupo_list <- map(reg_gr_bio_list, function(x){
    #x <- reg_gr_bio_list[[3]]
    #x <- reg_gr_bio_list[[4]]
    if(verbose){
      message("  Grupo ", i," de ",nrow(reg_gr_bio),": " ,x$slug)
    }
    i <<- i + 1
    x <- as.list(x)
    current_slug <- x$slug
    x$parent <- reg_gr_bio_parent |>
      filter(slug == current_slug) |>
      collect()

    x$estimadas <- estimadas_grupo(x$slug, con)

    grupo <- x$slug
    subgrupo_especies <- sib_region_subgrupo(region, grupo, con) |>
      select(slug_grupo, label_grupo, especies_region_total) |>
      collect()
    x$subgrupo_especies <- subgrupo_especies

    species_list <- list_species(region, grupo = x$slug, con = con)
    species_list_top <- species_list |>
      #collect() |>
      #arrange(desc(registros)) |>
      slice_max(registros, n = 10) |>
      collect()
    x$species_list_top <- species_list_top

    tematicas <- c("amenazadas-nacional", "amenazadas-global", "cites", "migratorias",
                   "endemicas", "exoticas")
    species_list_tematica <- map(tematicas, function(tem){
      #tem <- tematicas[1]
      #tem <- "exoticas"
      #message(tem)
      spe <- list_species(region, grupo = x$slug, tematica = tem, con = con)
      spe_top <- spe |>
        slice_max(registros, n = 10) |>
        collect()
      spe_top
    })
    names(species_list_tematica) <- tematicas
    x$species_list_tematica <- species_list_tematica
    x
  })
  reg_grupo_list
}
