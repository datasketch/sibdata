#' Run Shiny application
#'
#' Ejecuta la aplicación Shiny del paquete sibdata.
#'
#' @return Invisible, ejecuta la aplicación Shiny en el puerto 3838.
#'
#' @export
run_app <- function() {
  app_file <- system.file("sib-data-app/app.R", package = "sibdata")
  shiny::runApp(app_file, port = 3838)
}
