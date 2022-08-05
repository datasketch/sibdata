
#' @export
profile_data <- function(slug, type){
  validate_profile_type(type)
}



#' @export
navigation_trees <- function(type, json_file = NULL){

  sib_validate_profile_type(type)

  #type <- "region"
  #type <- "grupo_biologico"
  #type <- "tematica"

  table <- sib_tables(type)
  if("activa" %in% names(table)){
    table <- table |> dplyr::filter(activa)
  }

  # Add icon URLs
  if("icon" %in% names(table)){
    table <- table |>
      dplyr::mutate(icon_white = paste0("static/icons/",slug,"-white.svg"),
             icon_black = paste0("static/icons/",slug,"-black.svg")
             )
  }

  if(type == "tematica"){
    table <- table |>
      dplyr::filter(is.na(orden))
  }

  table

  tree <- data.tree::FromDataFrameNetwork(table)

  l <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "slug",
                      childrenName = "children")
  if(!is.null(json_file)){
    jsonlite::write_json(l, json_file,
                         auto_unbox = TRUE, pretty = TRUE)
  }
  l
}


publicadores_to_json <- function(json_file){
  pubs <- sib_tables("publicador") |>
    dplyr::distinct()
  jsonlite::write_json(pubs, json_file,
                       auto_unbox = TRUE, pretty = TRUE)
}





