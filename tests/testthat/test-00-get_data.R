test_that("Check temática", {

  # test_dir <- here::here()
  # warning(test_dir)
  # tematica <- read_delim("data-raw/db-cifras-sib/tematica.tsv")
  tematica <- read_delim("../../data-raw/db-cifras-sib/tematica.tsv")
  tematica <- tematica |>
    filter(activa == "TRUE") |>
    collect()
  tree <- data.tree::FromDataFrameNetwork(tematica)
  expect_true("Node" %in% class(tree))
  l <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "slug",
                                 childrenName = "children")
  expect_equal(l$slug, "0")


  # Exoticas total
  expect_true("exoticas-total" %in% tematica$parent)


  ##




})


test_that("test tree", {

  con <- DBI::dbConnect(duckdb::duckdb(), sys_file_sibdata("db/sibdata.duckdb"),
                        read_only = TRUE)
  tematica <- sibdata_tematica(con = con) |> collect()

  tematica <- tematica |>
    filter(activa == "TRUE") |>
    collect()
  tree <- data.tree::FromDataFrameNetwork(tematica)
  expect_true("Node" %in% class(tree))
  l <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "slug",
                                 childrenName = "children")
  expect_equal(l$slug, "0")


  # Exoticas total
  expect_true("exoticas-total" %in% tematica$parent)


})


