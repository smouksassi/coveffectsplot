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

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Thank you for using coveffectsplot!\nif you find it useful, please cite as:")
    packageStartupMessage("JF Marier, N Teuscher and MS Mouksassi. Evaluation of covariate effects using forest plots and introduction to the coveffectsplot R package. CPT Pharmacometrics Syst Pharmacol. 2022;11:1283–1293. doi:10.1002/psp4.12829")
}


