library(sibdata)


# generate_navigation
navigation_trees("region",
                 json_file = "static/data/nav_region.json")

navigation_trees("grupo_biologico",
                 json_file = "static/data/nav_grupo_biologico.json")

navigation_trees("grupo_interes_conservacion",
                 json_file = "static/data/nav_grupo_interes_conservacion.json")

navigation_trees("tematica",
                 json_file = "static/data/nav_tematica.json")

publicadores_to_json("static/data/publicador.json")


# Generate files for regions

av_regions <- sib_available_regions()

map(av_regions, function(region){
  # region <- "narino"
  subregs <- sib_available_subregions(region)
  parent <- sib_parent_region(region)

  reg_gr_bio <- sib_tables("region_grupo_biologico") |>
    filter(slug_region == region)
  reg_gr_int <- sib_tables("region_grupo_interes_conservacion") |>
    filter(slug_region == region)
  reg_tematica <- sib_tables("region_tematica") |>
    filter(slug_region == region)
  subreg_tematica <- sib_tables("region_tematica") |>
    filter(slug_region %in% subregs)
  parent_tematica <- sib_tables("region_tematica") |>
    filter(slug_region == parent)

  l <- list(
    tematica = subreg_tematica,
    grupos_biologicos = reg_gr_bio,
    grupos_interes = reg_gr_int,
    regiones = subreg_tematica
    )
  jsonlite::write_json(l, paste0("static/data/",region, ".json"),
                   auto_unbox = TRUE, pretty =TRUE)
})






