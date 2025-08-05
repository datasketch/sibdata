



get_tematicas_tree <- function(con){
  tematica <- sibdata_tematica(con) |> collect()
  tree <- data.tree::FromDataFrameNetwork(tematica)
  l <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "slug",
                                 childrenName = "children")

  l
}
