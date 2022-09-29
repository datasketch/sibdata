library(sibdata)
library(lfltmagic)

devtools::load_all()

#save_info_page("static/data")


# Generate navigation files

# Generate files for regions

av_regions <- sib_available_regions(subtipo = c("Departamento", "País"))

library(tictoc)

here::dr_here()
#here::set_here("./..")
setwd("../")
here::dr_here()
tic()

map(av_regions, function(region){
  message(region)
  # region <- "boyaca"
  # region <- "narino"
  # region <- "tolima"
  # region <- "colombia"

  nav_tematica <- navigation_trees("tematica")
  nav_grupo_biologico <- navigation_trees("grupo_biologico")
  nav_grupo_interes <- navigation_trees("grupo_interes")
  nav_territorio <- navigation_trees("territorio", region = region)


  general_info <- sib_region_general(region)

  gallery <- make_gallery(region)

  slides <- make_region_slides(region)
  #slides <- list()

  reg_gr_bio <- region_grupo_data(region, tipo = "biologico", verbose = TRUE)
  reg_gr_int <- region_grupo_data(region, tipo = "interes", verbose = TRUE)


  # Temáticas

  tem_list <- tematica_list(region)
  #tem_list <- NA

  # Territorio
  dir.create(glue::glue("static/charts/{region}"))

  subreg_tematica <- subregion_tematica(region)
  d <- subreg_tematica |>
    collect()

  dd <- d |>
    select(slug_region, especies_region_total, registros_region_total)

  if(region == "colombia"){
    map_name <- "col_departments"
    deptos <- sibdata_departamento() |> collect()
    dd <- dd |>
      left_join(deptos, by = c("slug_region" = "slug"), copy = TRUE)

  }else{
    map_name <- paste0("col_depto_", region)
    munis <- sibdata_municipio() |> collect()
    dd <- dd |>
      left_join(munis, by = c("slug_region" = "slug"))
  }

  dd_esp <- dd |> select(cod_dane, value = especies_region_total, label)
  dd_reg <- dd |> select(cod_dane, value = registros_region_total, label)

  tooltip <- "<b>{value} especies</b><br><i>{label}</i>"
  munis_chart1<- lfltmagic::lflt_choropleth_GcdNum(dd_esp, map_name = map_name,
                                                  tooltip = tooltip)

  #munis_chart1 <- sib_chart_reg_municipios(d, "especies_region_total")
  path1 <- glue::glue("static/charts/{region}/region_municipios_1.html")
  htmlwidgets::saveWidget(munis_chart1, path1)

  #munis_chart2 <- sib_chart_reg_municipios(d, "registros_region_total")
  tooltip <- "<b>{value} observaciones</b><br><i>{label}</i>"
  munis_chart2<- lfltmagic::lflt_choropleth_GcdNum(dd_reg, map_name = map_name,
                                                   tooltip = tooltip)
  path2 <- glue::glue("static/charts/{region}/region_municipios_2.html")
  htmlwidgets::saveWidget(munis_chart2, path2)

  region_tipo <- "municipio"
  if(region == "colombia") region_tipo <- "departamento"
  territorio <- list(
    list(
      slug = "municipios",
      label = "Municipios",
      charts = list(
        list(title = glue::glue("Especies por {region_tipo}"), path = path1, layout = "title/chart"),
        list(title =  glue::glue("Registros por {region_tipo}"), path = path2, layout = "title/chart")
      )
    ),
    list(
      slug = "areas-protegidas",
      label = "Áreas protegidas",
      title = "Próximamente tendrás acceso a la información de las áreas protegidas",
      charts = list()
    ),
    list(
      slug = "ecosistemas-estrategicos",
      label = "Ecosistemas estratégicos",
      title = "Próximamente tendrás acceso a la información de ecosistemas estratégicos",
      charts = list()
    )
  )

  ##
  patrocinadores <- sibdata_patrocinador()
  patrocinador <- sibdata_region_patrocinador() |>
    filter(slug_region == region)
  patrocinador <- patrocinador |>
    left_join(patrocinadores, by = c("slug_patrocinador" = "slug")) |>
    collect()

  publicadores <- sibdata_region_publicador() |>
    filter(slug_region == region) |>
    left_join(sibdata_publicador(), by = c("slug_publicador" = "slug")) |>
    select(slug_publicador, registros = registros.x, especies = especies.x,
           label, pais_publicacion,
           url_logo, url_socio) |>
    arrange(desc(registros)) |>
    collect()



  municipios_lista <- subreg_tematica |>
    select(slug =slug_region, label) |>
    collect()

  l <- list(
    general_info = general_info,
    nav_tematica = nav_tematica,
    nav_grupo_biologico = nav_grupo_biologico,
    nav_grupo_interes = nav_grupo_interes,
    nav_territorio = nav_territorio,

    gallery = gallery,
    slides = slides,
    tematica = tem_list,
    grupos_biologicos = reg_gr_bio,
    grupos_interes = reg_gr_int,

    territorio = territorio,

    patrocinador = patrocinador,
    publicadores = publicadores,
    municipios_lista = municipios_lista
  )
  dir.create(file.path("static/data",region))
  jsonlite::write_json(l, paste0("static/data/",region,"/",region, ".json"),
                       auto_unbox = TRUE, pretty =TRUE)



})


toc()




