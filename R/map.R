
#' @export
choropleth_map <- function(data = NULL,
                           region = NULL,
                           tipo = NULL,
                           cobertura = NULL,
                           tematica = NULL,
                           indicador = NULL,
                           grupo = NULL,
                           n_especies = FALSE,
                           all_indicators = FALSE,
                           palette_numeric = NULL,
                           con = NULL,
                           conmap = NULL,
                           debug = FALSE, ...) {

  if (debug) {
    message("🗺️ CHOROPLETH_MAP CALLED:")
    message("- tipo: ", tipo)
    message("- indicador: ", indicador)
    message("- region: ", region)
  }

  no_conmap <- is.null(conmap)
  region_especial <- FALSE

  if(is.null(conmap)){
    conmap <- gt_con(conmap)
  }

  inp <- as.list(environment())

  inp$tidy <- TRUE
  inp$subregiones <- TRUE # only for Colombia and departments

  palette_numeric <- palette_numeric %||% c("#b6ecbf", "#29567d")
  if(!is.null(inp$indicador)){
    if(grepl("amenaza.*_cr", inp$indicador))
      palette_numeric <- c("#f9c9c9", "#d9453d")
    if(grepl("amenaza.*_en", inp$indicador))
      palette_numeric <- c("#ffe9d9", "#d8783d")
    if(grepl("amenaza.*_vu", inp$indicador))
      palette_numeric <- c("#fff9d9", "#d7a900")
    if(grepl("cites.*_iii$", inp$indicador))
      palette_numeric <- c("#daf2cc", "#4DD3AC")
    if(grepl("cites.*_ii$", inp$indicador))
      palette_numeric <- c("#fff9d9", "#FFD150")
    if(grepl("cites.*_i$", inp$indicador))
      palette_numeric <- c("#daf0ff", "#00AFFF")
    if(grepl("cites.*_i_ii$", inp$indicador))
      palette_numeric <- c("#dcdcdc", "#000000")
  }

  if(is.null(inp$region))
    stop("Need a region to plot map")

  region <- inp$region
  if(region == "colombia"){
    inp$subregiones <- TRUE
    map_name <- "col_departments"
    region_codes <- sibdata_departamento(con) |>
      select(slug_region = slug, cod_dane) |>
      collect()
  } else if (region %in% sib_available_regions(subtipo = "Departamento", con = con)){
    inp$subregiones <- TRUE
    region_id <- gsub("-", "_", region)
    if(region_id == "norte_santander") region_id <- "norte_de_santander"
    if(region_id == "san_andres_providencia") region_id <- "san_andres_providencia_y_santa_catalina"
    if(region_id == "bogota_dc") region_id <- "bogota_d_c"
    region_codes <- sibdata_municipio(con) |>
      select(slug_region = slug, cod_dane) |> collect()
    map_name <- paste0("col_municipalities_",region_id)
  } else{
    regiones_especiales <- c(
      "region-amazonia",
      "reserva-forestal-la-planada",
      "resguardo-indigena-pialapi-pueblo-viejo"
    )
    region_especial <- TRUE
    inp$subregiones <- FALSE
    if(region %in% regiones_especiales){
      geo_path <- glue::glue("geo/{region}.geojson")
      sf <- sf::st_read(sys_file_sibdata(geo_path), quiet = TRUE)
      lf <- basic_map(sf)
        return(lf)
    }else{
      stop("No valid region")
    }
  }


  if(!is.null(data)){
    d <- data
  } else{
    d <- sibdata(inp$region,
                 grupo = inp$grupo,
                 tipo = inp$tipo,
                 cobertura = inp$cobertura,
                 tematica = inp$tematica,
                 indicador = inp$indicador,
                 subregiones = inp$subregiones,
                 with_parent = inp$with_parent,
                 con = con)

  }

  # if(inp$tipo == "especies"){
  #   d <- d |> filter(grepl("total", indicador))
  # }
  # str(inp)

  ## Only calculate sf merge d with Colombia and deptos
  if(!is.null(inp$indicador)){
    val <- inp$indicador
  }else{
    #val <- inp$tematica
    val <- "count"
  }

  if(region == "bogota-dc"){
    d$label <- "BOGOTÁ"
  }
  # str(d)

  cols <- c("slug_region", "label", val)


  d0 <- d |>
    select(all_of(cols)) |>
    left_join(region_codes) |>
    rename(value = val)
  # select(name = label, value = val) |>
  # mutate(name = toupper(name)) |>
  # filter(!is.na(value))
  # d0$name[d0$name == "BOGOTÁ, D. C."] <- "BOGOTÁ"

  sf <- gt_sf(map_name, con = conmap) |>
    rename_dotdot()
  # if(nrow(d0) > 1.5 * nrow(sf)){
  #   warning("Data may have repeated geographic rows, taking the first indicator found")
  #   # Remove duplicates by taking the first occurrence of each geographic name
  #   d0 <- d0 %>%
  #     group_by(name) %>%
  #     slice(1) %>%
  #     ungroup()
  # }

  if(nrow(d0) > 0){
    # # message("nrow d0: ", nrow(d0))
    # dmatch <- gt_match(d0, map_name, unique = TRUE, con = conmap) |>
    #   select(name, value, "..gt_id")

    dgeo <- sf |>
      left_join(d0, by = c("..gt_id" = "cod_dane"))
    #|>
    #  select()
  }else{
    dgeo <- sf
    dgeo$value <- NA
  }

  # Shutdown connection if it wasn't originally provided
  if(no_conmap){
    gt_discon(conmap)
  }


  pal <- leaflet::colorNumeric(
    palette = rev(palette_numeric),
    domain = d0$value * -1
  )

  # Build human-friendly legend title
  inds_for_title <- if (!is.null(inp$indicador)) inp$indicador else unique(d$indicador)
  # Normalize 'observaciones_' slugs to dictionary keys ('registros_') before merging labels
  inds_for_title <- gsub("^observaciones_", "registros_", inds_for_title)
  # Merge labels for each indicator and collapse if multiple
  title_labels <- tryCatch({
    sib_merge_ind_label(inds_for_title, con = con)
  }, error = function(e){
    inds_for_title
  })
  title <- dstools::collapse(unique(title_labels))
  # Use Observaciones instead of registros in display
  title <- gsub("[Rr]egistros", "Observaciones", title)

  # fix names
  dgeo <- dgeo |>
    mutate(name = ..gt_name)



  # str(dgeo)

  # Create the leaflet map
  lt <- leaflet::leaflet(dgeo) |>
    leaflet::addPolygons(
      fillColor = ~pal(dgeo$value * -1),
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
      label = ~ifelse(is.na(dgeo$value),
                      dgeo$name,
                      paste0(dgeo$label, ": ", dgeo$value)),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    )

  if(nrow(dgeo) > 1){
    lt <- lt |>
      leaflet::addLegend(
        pal = pal,
        values = dgeo$value * -1,
        title = title,
        position = "bottomright",
        bins = 5,
        labFormat = leaflet::labelFormat(
          transform = function(x) -1 * x
        )
      )
  }
   lt |>
     leaflet.extras::setMapWidgetStyle(list(background = "#ffffff")) #|>
  #   leaflet::addProviderTiles("")
}


#' Create basic map
#'
#' Crea un mapa básico sin datos asociados.
#'
#' @param sf Objeto `sf` con datos geoespaciales.
#'
#' @return Objeto Leaflet con mapa básico.
#'
#' @keywords internal
basic_map <- function(sf) {


  bounds <- as.vector(sf::st_bbox(sf))

  leaflet::leaflet(sf) |>
    leaflet::addPolygons(
      fillColor = "#349434",
      weight = 1,
      opacity = 1,
      color = "white",
      label = sf$label,
      options = leaflet::leafletOptions(
        zoomControl = FALSE,
        dragging = FALSE,
        doubleClickZoom = FALSE,
        scrollWheelZoom = FALSE,
        minZoom = 3, maxZoom = 3
        )
    ) |>
    leaflet::fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) |>
    leaflet.extras::setMapWidgetStyle(list(background = "#ffffff")) |>
    leaflet::addProviderTiles("", options = list(attribution = ""))

}

