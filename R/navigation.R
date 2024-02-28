
#' @export
navigation_trees <- function(type, region = NULL, con = con){

  #type <- "region"
  #type <- "grupo_biologico"
  #type <- "tematica"
  #type <- "territorio"
  sib_validate_profile_type(type)


  if(type %in% c("grupo_biologico", "grupo_interes")){
    table <- sibdata_grupo(con)
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
    table <- sibdata_territorio(con) |>
      filter(slug_region == region | parent == region)

  } else if(type == "tematica") {
    table <- sibdata_tematica(con) |>
      filter(parent != "cites") |>
      filter(parent != "amenazadas-global") |>
      filter(parent != "amenazadas-nacional") |>
      filter(parent != "exoticas-invasoras")
  } else if(type == "region") {
    table <- sibdata_region(con)
  } else{
    stop("Undefined type")
  }

  if("activa" %in% colnames(table)){
    table <- table |> dplyr::filter(activa)
  }

  # Add icon URLs
  if("icon" %in% colnames(table)){
    table <- table |>
      dplyr::mutate(icon_white = paste0("static/icons/",slug,"-white.svg"),
                    icon_black = paste0("static/icons/",slug,"-black.svg")
      )
  }

  table <- table |> collect()
  if(nrow(table) == 0) return(list())
  tree <- data.tree::FromDataFrameNetwork(table)

  l <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "slug",
                                 childrenName = "children")

  l
}

