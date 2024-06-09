
#' @export
sibdata_indicadores <- function(con){
  sibdata_ind_meta(con) |> collect() |>
    mutate(subtematica = paste(tematica, categorias_tematicas, sep = "_"))
}
