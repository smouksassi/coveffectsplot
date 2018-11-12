#' Run the interactiveforestplot application
#'
#' Run the \code{interactiveforestplot} application.
#'
#' @examples
#' if (interactive()) {
#'   run_interactiveforestplot()
#' }
#' @export
run_interactiveforestplot <- function() {
  appDir <- system.file("shiny", package = "interactiveforestplot")
  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `interactiveforestplot`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
