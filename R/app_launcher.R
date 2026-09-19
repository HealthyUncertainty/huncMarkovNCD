#' Launch the Interactive Shiny App
#'
#' Opens the interactive cost-effectiveness interface for the fictional
#' zelravine versus usual care NCD model. Requires the \pkg{shiny} package
#' (and, for the ICER table, \pkg{DT} and \pkg{dampack}).
#'
#' @param ... Arguments passed to \code{shiny::runApp()}
#' @return Called for its side effect of launching the app
#' @export
#' @examples
#' \dontrun{
#' launch_app()
#' }
launch_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with install.packages('shiny').",
         call. = FALSE)
  }
  app_dir <- system.file("shiny-app", package = "huncMarkovNCD")
  if (!nzchar(app_dir)) {
    stop("Could not find the Shiny app directory. Try re-installing 'huncMarkovNCD'.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
