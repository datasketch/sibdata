
#' @export
profile_data <- function(slug, type){
  validate_profile_type(type)
}



#' @export
navigation_trees <- function(type, json_file = NULL){

  validate_profile_type(type)

  table <- sib_tables(type)

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
    distinct()
  jsonlite::write_json(pubs, json_file,
                       auto_unbox = TRUE, pretty = TRUE)
}




possible_profile_types <- function(){
  c("region", "grupo_biologico", "grupo_interes_conservacion", "specie", "tematica")
}

validate_profile_type <- function(type){
  if(!type %in% possible_profile_types())
    stop("Type must be one of: ", paste(possible_profile_types(), collapse = ", "))
}

