test_that("Merge", {

  d <- sibdata_region_publicador() |>
    select(slug_region, slug_publicador)
  m <- sib_merge_region_label(d, slug = "slug_region")
  expect_equal(colnames(m), c("slug_region", "label_region",  "slug_publicador"))

})
