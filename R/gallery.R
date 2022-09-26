
make_gallery <- function(region){

  txts <- sibdata_dato_relevante() |>
    filter(slug_region == region) |>
    select(text = descripcion) |>
    collect()

  imgs <- sibdata_gallery_images() |>
    filter(slug_region == region) |>
    select(image = img_link) |>
    collect()

  n <- min(nrow(txts), nrow(imgs))
  txts <- txts |> slice(1:n)
  imgs <- imgs |> slice(1:n)

  gal <- bind_cols(txts, imgs) |>
    pivot_longer(text:image) |>
    rownames_to_column(var = "id") |>
    pivot_wider("id", names_from = "name", values_from = "value")

  gal
}
