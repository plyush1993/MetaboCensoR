#' Run the MetaboCensoR Application
#'
#' Run the MetaboCensoR Application locally. Shiny deployment
#' \href{https://plyush1993.shinyapps.io/metabocensor/}{here}.
#' @param max_size_gb Numeric. The maximum allowed file upload size in GBs. Defaults to 5.
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @references To be updated.
#'
#' @importFrom crayon blue red cyan bold %+%
#' @export
run_metabocensor <- function(max_size_gb = 5, ...) {
  cat("\n")
  cat(crayon::cyan("             +--------------------+\n"))
  app_name <- paste0(
    crayon::blue(crayon::bold("   Metabo")),
    crayon::red(crayon::bold("CensoR   "))
  )
  cat(crayon::cyan("             | "), app_name, crayon::cyan(" |\n"), sep = "")
  cat(crayon::cyan("             +--------------------+\n"))
  cat("\n")
  cat(crayon::cyan(crayon::bold("Shiny App for filtering redundant features in LC-MS\n")))
  cat("\n")

  flush.console()

  old_opts <- options(shiny.maxRequestSize = max_size_gb * 1024^3)
    on.exit({
    options(old_opts)
    gc()
    }, add = TRUE)

  shiny::addResourcePath("www", system.file("www", package = "MetaboCensoR"))
  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
