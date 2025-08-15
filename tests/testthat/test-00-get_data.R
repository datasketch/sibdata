test_that("Check temática", {

  # test_dir <- here::here()
  # warning(test_dir)
  tematica <- read_delim("../../data-raw/db-cifras-sib/tematica.tsv")
  tematica <- tematica |>
    filter(activa == 1) |>
    collect()
  tree <- data.tree::FromDataFrameNetwork(tematica)
  expect_true("Node" %in% class(tree))
  l <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "slug",
                                 childrenName = "children")
  expect_equal(l$slug, "0")

})
