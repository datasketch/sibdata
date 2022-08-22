
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

#' @export
sib_validate_list_especies <- function(esps, region = NULL, grupo = NULL,
                                       tematica = NULL,
                                       validate = "warning"){
    n_esp <- sibdata(region, grupo = grupo, n_especies = TRUE)
    msg1 <- glue::glue("Validando número especies: "," region=",region, "grupo=",grupo, ," \n")
    message(msg1)
    if(nrow(esps) != n_esp){
      msg <- waldo::compare(nrow(esps), n_esp, x_arg = "n list", y_arg = "sibdata")
      if(validate == "warning") warning(msg)
      if(validate == "error") stop(msg)
    }
}
