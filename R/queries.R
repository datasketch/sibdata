

tematica_list <- function(region){

  reg_tematica <- region_tematica(region)
  esp_reg <- sib_tables("especie_region") |>
    dplyr::filter(slug_region == region)


  esp_tem <- sib_tables("especie_tematica")
  esp_reg_tem <-  esp_reg |>
    dplyr::left_join(esp_tem) |>
    dplyr::select(-registros)


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
    #x <- tems_list[[8]]
    x$slug <- x$slug_tematica
    esps_tem <- esp_reg_tem |>
      dplyr::select(-slug_region) |>
      dplyr::filter(grepl(x$slug, slug_tematica)) #|>
    #distinct(slug_especie, .keep_all = TRUE)
    #x$especies <- list(esps_tem)
    x$title <- paste0("Lista especies: ", x$slug)
    #chart <- sib_chart_gt_table2(esps_tem)
    slug <- x$slug
    path <- glue::glue("static/charts/{region}/{slug}.html")
    #gt::gtsave(chart, path)
    x$chart <- path
    as.list(x)
  })

  tematica_list

}






region_gr_bio_data <- function(region){
  reg_gr_bio <- sib_tables("region_grupo_biologico") |>
    dplyr::filter(slug_region == region) |>
    dplyr::mutate(slug = slug_grupo_biologico) |>
    dplyr::relocate(slug)

  #keys <- reg_gr_bio |> group_by(slug) |> group_keys()
  reg_gr_bio_list <- reg_gr_bio |>
    group_split(slug)
  reg_gr_bio_list <- map(reg_gr_bio_list, function(x){
    #x <- reg_gr_bio_list[[1]]
    x <- as.list(x)
    species_list <- list_species(region, grupo_interes = x$slug)
    species_list_top <- species_list |>
      arrange(desc(registros)) |>
      slice(1:50)
    x$species_list_top <- species_list_top
    species_list_bottom <- species_list |>
      arrange(registros) |>
      slice(1:50)
    x
  })
  reg_gr_bio_list
}

region_gr_int_data <- function(region){
  reg_gr_int <- sib_tables("region_grupo_interes_conservacion") |>
    dplyr::filter(slug_region == region) |>
    dplyr::mutate(slug = slug_grupo_interes_conservacion) |>
    dplyr::relocate(slug)

  reg_gr_int_list <- reg_gr_int |>
    group_split(slug)
  reg_gr_int_list <- map(reg_gr_int_list, function(x){
    #x <- reg_gr_bio_list[[1]]
    x <- as.list(x)
    species_list <- list_species(region, grupo_interes = x$slug)
    species_list_top <- species_list |>
      arrange(desc(registros)) |>
      slice(1:50)
    x$species_list_top <- species_list_top
    species_list_bottom <- species_list |>
      arrange(registros) |>
      slice(1:50)
    x$species_list_bottom <- species_list_bottom
    x
  })
  reg_gr_int_list
}

region_tematica <- function(region){
  reg_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == region)
  reg_tematica
}

subregion_tematica <- function(region){
  subregs <- sib_available_subregions(region)
  subreg_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region %in% subregs)
  subreg_tematica
}

parent_tematica <- function(region){
  parent <- sib_parent_region(region)
  parent_tematica <- sib_tables("region_tematica") |>
    dplyr::filter(slug_region == parent)
  parent_tematica
}



