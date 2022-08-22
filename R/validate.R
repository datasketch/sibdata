
#' @export
sib_validate_profile_type <- function(type){
  if(!type %in% sib_available_profile_types())
    stop("Type must be one of: ", paste(sib_available_profile_types(), collapse = ", "))
}




#' @export
sib_validate_available_regions <- function(region){
  if(!region %in% sib_available_regions())
    stop("Type must be one of: ",
         paste(sib_available_regions(with_grupo = with_grupo), collapse = ", "))
}


