#' Launch Shiny App
#'
#' @param name The name of the app to run
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
app <- function(name = "RMDitemfit", ...) {
  appDir <- system.file(paste0("apps/", name), package = "RASCHplot")
  if (appDir == "") stop("The shiny app ", name, " does not exist")
  shiny::runApp(appDir, ...)
}
#app <- function(name = "RMDitemfit", ...) {
#  appDir <- system.file(paste0("apps/", name, "/"), package = "RASCHplot")
#  #if (appDir == "") stop("The shiny app ", name, " does not exist")
#  #shiny::runApp(appDir, ...)
#  if (name == "RMDitemfit") {
#    source(paste0(appDir, "server.R"))
#    source(paste0(appDir, "ui.R"))
#    shiny_env <- new.env()
#    environment(RMDui) <- shiny_env
#    environment(RMDserver) <- shiny_env
#    myapp <- shiny::shinyApp(
#      ui = RMDui,
#      server = RMDserver
#    )
#    shiny::runApp(myapp, ...)
#  }
#  if (name == "RMPitemfit") {
#    shiny::shinyApp(ui = RMPui,
#                    server = RMPserver)
#  }
#}



