
make_gallery <- function(region){

  txts <- sib_tables("dato_relevante") |>
    filter(slug_region == region) |>
    select(text = descripcion)

  imgs <- sib_tables("gallery_images") |>
    filter(slug_region == region) |>
    select(image = img_link)

  n <- min(nrow(txts), nrow(imgs))
  txts <- txts |> slice(1:n)
  imgs <- imgs |> slice(1:n)

  gal <- bind_cols(txts, imgs) |>
    pivot_longer(text:image) |>
    rownames_to_column(var = "id") |>
    pivot_wider("id", names_from = "name", values_from = "value")

  gal
}
