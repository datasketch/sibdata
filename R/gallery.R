#' Create gallery data for a region
#'
#' Genera datos de galería (textos e imágenes) para una región específica.
#'
#' @param region Slug de la región (debe estar en la lista de regiones válidas).
#' @param con Conexión a la base de datos.
#'
#' @return Data frame con columnas `text`, `image` y `credit` combinando textos
#'   relevantes e imágenes de la galería.
#'
#' @export
make_gallery <- function(region, con) {
  gallery_regions <- c(
    "colombia",
    "boyaca",
    "narino",
    "tolima",
    "santander",
    "region-amazonia"
  )
  if (!region %in% gallery_regions) {
    warning(
      "Trying create a gallery in non-valid region. Valid regions:\n",
      toString(gallery_regions)
    )
    return(list())
  }

  txts <- sibdata_dato_relevante(con) |>
    filter(slug_region == region) |>
    select(text = descripcion) |>
    collect()

  imgs <- sibdata_gallery_images(con) |>
    filter(slug_region == region) |>
    select(image = img_link, credit = credito) |>
    collect()

  n <- min(nrow(txts), nrow(imgs), 6)
  txts <- txts |> slice(1:(n))
  imgs <- imgs |> slice(c(1:(n)))

  gal <- cbind(txts, imgs)


  gal
}
