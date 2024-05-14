
#' @export
sibdata_indicadores <- function(con){
  sibdata_ind_meta(con) |> collect()
}
