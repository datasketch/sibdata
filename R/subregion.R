
subregion_tematica <- function(region){

  regs <- sibdata_region() |>
    select(slug_region = slug, label)

  subregs <- sib_available_subregions(region)
  subreg_tematica <- sibdata_region_tematica() |>
    dplyr::filter(slug_region %in% subregs) |>
    dplyr::left_join(regs, by = "slug_region") |>
    dplyr::select(-fecha_corte) |>
    dplyr::relocate(slug_region, label, everything())
  subreg_tematica
}

