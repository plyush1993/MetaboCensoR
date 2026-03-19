#' Run the MetaboCensoR Application
#'
#' @importFrom crayon blue red cyan bold %+%
#' @export
run_metabocensor <- function(...) {
  cat("\n")
  cat(crayon::cyan("             ╔════════════════════╗\n"))
  cat(crayon::cyan("             ║ "), crayon::blue$bold('   Metabo' %+% red$bold('CensoR   ')), crayon::cyan(" ║\n"), sep = "")
  cat(crayon::cyan("             ╚════════════════════╝\n"))
  cat("\n")
  cat(crayon::cyan$bold("Shiny App for filtering redundant features in LC-MS\n"))
  cat("\n")

  flush.console()

  old_opts <- options(shiny.maxRequestSize = 5 * 1024^3)
    on.exit({
    options(old_opts)
    gc()
    }, add = TRUE)

  shiny::addResourcePath("www", system.file("www", package = "MetaboCensoR"))
  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
