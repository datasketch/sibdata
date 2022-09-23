
#' @export
profile_data <- function(slug, type){
  validate_profile_type(type)
}



#' @export
navigation_trees <- function(type, region = NULL, json_file = NULL){

  sib_validate_profile_type(type)

  #type <- "region"
  #type <- "grupo_biologico"
  #type <- "tematica"
  #type <- "territorio"

  if(type %in% c("grupo_biologico", "grupo_interes")){
    table <- sib_tables("grupo")
    if(type == "grupo_biologico"){
      table <- table |>
        filter(tipo == "biologico") |>
        select(-tipo)
    }
    if(type == "grupo_interes"){
      table <- table |>
        filter(tipo == "interes") |>
        select(-tipo)
    }
  } else if(type == "territorio"){
    table <- sib_tables("territorio") |>
      filter(slug_region == region | parent == region)

  } else if(type == "tematica") {
    table <- sib_tables(type) |>
      filter(parent != "cites")
  } else {
    table <- sib_tables(type)
  }

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

  # if(type == "tematica"){
  #   table <- table |>
  #     dplyr::filter(is.na(orden))
  # }

  #table

  tree <- data.tree::FromDataFrameNetwork(table)

  l <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "slug",
                                 childrenName = "children")
  if(!is.null(json_file)){
    jsonlite::write_json(l, json_file,
                         auto_unbox = TRUE, pretty = TRUE)
  }
  l
}

#' @export
publicadores_to_json <- function(json_file){

  pub_col <- sib_tables("region_publicador") |>
    filter(slug_region == "colombia") |>
    select(slug = slug_publicador, registros, especies)
  pub_reg <- sib_tables("region_publicador") |>
    filter(slug_region %in% c("boyaca", "narino", "tolima", "santander", "colombia")) |>
    sib_merge_region_label() |>
    select(slug = slug_publicador, label) |>
    distinct() |>
    group_by(slug) |>
    summarise(region = paste(label, collapse = ", "))

  pubs <- sib_tables("publicador") |>
    dplyr::distinct() |>
    left_join(pub_col) |>
    left_join(pub_reg)

  jsonlite::write_json(pubs, json_file,
                       auto_unbox = TRUE, pretty = TRUE)
}





