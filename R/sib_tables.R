
#' @export
sib_tables <- function(table = NULL){
  if(!table %in% sibdata::available_tables)
    stop("Table: ", table, " doesn't exists")
  ds <- sibdata::ds
  if(!is.null(table)){
    return(ds[[table]])
  }
  ds
}

