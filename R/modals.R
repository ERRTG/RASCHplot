#' Create a modal dialog UI when mean of item parameters is not zero
#'
#' @rdname zeromean_confirm
#' @export
zeromean_confirm <- function() {
  shiny::modalDialog(
    "Mean of item parameters is not zero! Are you sure you want to continue?",
    title = "Constraint",
    footer = shiny::tagList(
      shiny::actionButton("cancel", "Cancel"),
      shiny::actionButton("ok", "Continue", class = "btn btn-danger")
    )
  )
}
#' Create a modal dialog UI when files of item parameters and person parameters are the same
#'
#' @rdname nobsnitms_confirm
#' @export
nobsnitms_confirm <- function() {
  shiny::modalDialog(
    "The input files of item parameters and person parameters are the same! Are you sure you want to continue?",
    title = "Constraint",
    footer = shiny::tagList(
      shiny::actionButton("canceln", "Cancel"),
      shiny::actionButton("okn", "Continue", class = "btn btn-danger")
    )
  )
}