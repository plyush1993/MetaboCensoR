#' Run the MetaboCensoR Application
#' Online version: https://plyush1993.shinyapps.io/metabocensor/
#'
#' @export
run_metabocensor <- function(...) {
  shiny::addResourcePath("www", system.file("www", package = "MetaboCensoR"))
  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
