
#' @export
choropleth_map <- function(region = NULL,
                           tipo = NULL,
                           cobertura = NULL,
                           tematica = NULL,
                           indicador = NULL,
                           grupo = NULL,
                           subregiones = FALSE,
                           with_parent = FALSE,
                           tidy = TRUE,
                           n_especies = FALSE,
                           all_indicators = FALSE,
                           con = NULL,
                           conmap = NULL, ...) {

  no_conmap <- is.null(conmap)
  conmap <- geotable::gt_con(conmap)

  inp <- as.list(environment())

  if(region == "colombia"){
    inp$subregiones <- TRUE
    map_name <- "col_departments"
  } else if (region %in% sib_available_regions(subtipo = "Departamento", con = con)){
    inp$subregiones <- TRUE
    region_id <- gsub("-", "_", region)
    if(region_id == "norte_santander") region_id <- "norte_de_santander"
    if(region_id == "san_andres_y_providencia") region_id <- "archipielago_de_san_andres_providencia_y_santa_catalina"
    if(region_id == "bogota_dc") region_id <- "bogota_d_c"
    map_name <- paste0("col_municipalities_",region_id)
  } else{
    return()
  }

  d <- sibdata(inp$region,
               grupo = inp$grupo,
               tipo = inp$tipo,
               cobertura = inp$cobertura,
               tematica = inp$tematica,
               indicador = inp$indicador,
               subregiones = inp$subregiones,
               with_parent = inp$with_parent,
               con = inp$con)

  # if(inp$tipo == "especies"){
  #   d <- d |> filter(grepl("total", indicador))
  # }
  if(!is.null(inp$indicador)){
    val <- inp$indicador
  }else{
    #val <- inp$tematica
    val <- "count"
  }

  d0 <- d |> select(name = label, value = val) |>
    mutate(name = toupper(name))
  dmatch <- geotable::gt_match(d0, map_name, con = conmap) |>
    select(name, value, "..gt_id")

  sf <- geotable::gt_sf(map_name, con = conmap) |>
    geotable::rename_dotdot()

  dgeo <- sf |> left_join(dmatch, by = "..gt_id")

  # Shutdown connection if it wasn't originally provided
  if(no_conmap){
    geotable::gt_discon(conmap)
  }

  # str(inp)

  pal <- leaflet::colorNumeric(
    palette = c("#b6ecbf", "#29567d"),
    domain = d0$value
  )

  title <- ifelse(!is.null(inp$indicador), inp$indicador, unique(d$indicador))
  title <- sib_merge_ind_label(title, con = con)

  # Create the leaflet map
  leaflet::leaflet(dgeo) |>
    leaflet::addPolygons(
      fillColor = ~pal(dgeo$value),
      weight = 1,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste0(dgeo$name, ": ", dgeo$value),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = dgeo$value,
      title = title,
      position = "bottomright"
    ) |>
    leaflet.extras::setMapWidgetStyle(list(background = "#ffffff")) |>
    leaflet::addProviderTiles("")
}

