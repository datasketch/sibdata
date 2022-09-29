#' @export
run_app <- function(){
  app_file <- system.file("sib-data-app/app.R", package = "sibdata")
  shiny::runApp(app_file, port = 3838)
}
