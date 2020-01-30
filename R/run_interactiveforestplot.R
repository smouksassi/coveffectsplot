#' Run the interactiveforestplot application
#'
#' Run the \code{interactiveforestplot} application.
#' @param data optional data to load when the app is launched
 
#' @examples
#' if (interactive()) {
#'   run_interactiveforestplot()
#' }
#' @export
run_interactiveforestplot <- function(data = NULL) {
  if (!is.null(data) && !is.data.frame(data)) {
    stop("data must be a data.frame", call. = FALSE)
  }
  appDir <- system.file("shiny", package = "coveffectsplot")
  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `coveffectsplot`.",
         call. = FALSE)
  }

  if (!is.null(data)) {
    .GlobalEnv$coveffectsplot_initdata <- data
    on.exit(rm(coveffectsplot_initdata, envir = .GlobalEnv))
  }
  shiny::runApp(appDir, display.mode = "normal")
}

# Make CRAN happy
if (getRversion() >= "2.15.1") utils::globalVariables(c("coveffectsplot_initdata"))