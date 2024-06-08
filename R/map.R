
#' @export
choropleth_map <- function(region,
                           tipo = NULL,
                           cobertura = NULL,
                           tematica = NULL,
                           grupo = NULL,
                           subregiones = FALSE,
                           with_parent = FALSE,
                           tidy = TRUE,
                           n_especies = FALSE,
                           all_indicators = FALSE,
                           con = NULL,
                           conmap = NULL) {

  # region <- "boyaca"
  inp <- list(...)

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
               subregiones = inp$subregiones,
               with_parent = inp$with_parent,
               con = con)
  if(inp$tipo == "especies"){
    d <- d |> filter(grepl("total", indicador))
  }

  d0 <- d |> select(name = label, value = count) |>
    mutate(name = toupper(name))
  sf <- gt_sf("col_departments")
  dgeo <- left_join(sf, d0)
  #dgeo <- gt_match(d, map_name)

  pal <- leaflet::colorNumeric(
    palette = c("#b6ecbf", "#29567d"),
    domain = d$count
  )

  # Create the leaflet map
  leaflet(dgeo) |>
    addPolygons(
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
    addLegend(
      pal = pal,
      values = dgeo$value,
      title = "Title",
      position = "bottomright"
    ) |>
    # setView(lng = -96, lat = 37.8, zoom = 4) %>%
    addProviderTiles("")
}

