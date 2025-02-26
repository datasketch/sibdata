
#' @export
choropleth_map <- function(data = NULL,
                           region = NULL,
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
                           palette_numeric = NULL,
                           con = NULL,
                           conmap = NULL, ...) {

  no_conmap <- is.null(conmap)
  conmap <- geotable::gt_con(conmap)

  inp <- as.list(environment())

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
  } else if (region %in% sib_available_regions(subtipo = "Departamento", con = con)){
    inp$subregiones <- TRUE
    region_id <- gsub("-", "_", region)
    if(region_id == "norte_santander") region_id <- "norte_de_santander"
    if(region_id == "san_andres_providencia") region_id <- "san_andres_providencia_y_santa_catalina"
    if(region_id == "bogota_dc") region_id <- "bogota_d_c"
    map_name <- paste0("col_municipalities_",region_id)
  } else{
    stop("No valid region")
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
                 con = inp$con)

  }

  # if(inp$tipo == "especies"){
  #   d <- d |> filter(grepl("total", indicador))
  # }
  # str(inp)
  if(!is.null(inp$indicador)){
    val <- inp$indicador
  }else{
    #val <- inp$tematica
    val <- "count"
  }

  if(region == "bogota-dc"){
    d$label <- "BOGOTÁ"
  }
  str(d)

  d0 <- d |> select(name = label, value = val) |>
    mutate(name = toupper(name)) |>
    filter(!is.na(value))
  d0$name[d0$name == "BOGOTÁ, D. C."] <- "BOGOTÁ"

  sf <- geotable::gt_sf(map_name, con = conmap) |>
    geotable::rename_dotdot()
  if(nrow(d0) > 1.5 * nrow(sf)){
    warning("Data may have repeated geographic rows, taking the first indicator found")
    return()
  }

  if(nrow(d0) > 0){
    # message("nrow d0: ", nrow(d0))
    dmatch <- geotable::gt_match(d0, map_name, unique = TRUE, con = conmap) |>
      select(name, value, "..gt_id")
    # message("dmatch")
    # message("is null dmatch", is.null(dmatch))
    # str(dmatch)
    dgeo <- sf |> left_join(dmatch, by = "..gt_id")
  }else{
    dgeo <- sf
    dgeo$value <- NA
  }

  # Shutdown connection if it wasn't originally provided
  if(no_conmap){
    geotable::gt_discon(conmap)
  }

  # str(inp)

  pal <- leaflet::colorNumeric(
    palette = palette_numeric,
    domain = d0$value
  )

  title <- ifelse(!is.null(inp$indicador), inp$indicador,
                  dstools::collapse(unique(d$indicador)))
  title <- sib_merge_ind_label(title, con = con)

  # fix names
  dgeo <- dgeo |>
    mutate(name = ..gt_name)

  str(dgeo)

  # Create the leaflet map
  lt <- leaflet::leaflet(dgeo) |>
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
      label = ~ifelse(is.na(dgeo$value),
                      dgeo$name,
                      paste0(dgeo$name, ": ", dgeo$value)),
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
        values = dgeo$value,
        title = title,
        position = "bottomright"
      )
  }

  lt |>
    leaflet.extras::setMapWidgetStyle(list(background = "#ffffff")) |>
    leaflet::addProviderTiles("")
}

