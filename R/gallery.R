
make_gallery <- function(region, con){

  gallery_regions <- c(
    "colombia",
    "boyaca",
    "narino",
    "tolima",
    "santander",
    "region-amazonia"
  )
  if(!region %in% gallery_regions){
    warning("Trying create a gallery in non-valid region. Valid regions:\n",
            toString(gallery_regions))
    return(list())
  }

  txts <- sibdata_dato_relevante(con) |>
    filter(slug_region == region) |>
    select(text = descripcion) |>
    collect()

  imgs <- sibdata_gallery_images(con) |>
    filter(slug_region == region) |>
    select(image = img_link) |>
    collect()

  if(region == "region-amazonia"){
    txts <- bind_rows(txts, txts) |> slice(1:8)
    imgs <- sibdata_gallery_images(con) |>
      filter(slug_region == "colombia") |>
      select(image = img_link) |>
      collect()
  }



  n <- min(nrow(txts), nrow(imgs), 7)
  txts <- txts |> slice(1:(n+1))
  imgs <- imgs |> slice(c(1:(n),1))

  gal <- bind_cols(txts, imgs) |>
    pivot_longer(text:image) |>
    rownames_to_column(var = "id") |>
    pivot_wider(id_cols = "id", names_from = "name", values_from = "value") |>
    slice(1:15)

  gal
}
