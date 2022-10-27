#' Launch Shiny App
#'
#' @param name The name of the app to run
#' @param ... arguments to pass to shiny::runApp
#'
#' @rawNamespace import(shiny, except = dataTableOutput)
#'
#' @export
#'
app <- function(name = "RMDitemfit", ...) {
  if (name == "RMDitemfit") {
    shiny::shinyApp(ui = RMDui,
                    server = RMDserver, ...)
  } else if (name == "RMPitemfit") {
    shiny::shinyApp(ui = RMPui,
                    server = RMPserver, ...)
  }
}
