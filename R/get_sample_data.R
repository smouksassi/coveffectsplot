#' Get sample dataset
#'
#' Get a sample dataset that is included with the package to plot a forest plot.
#'
#' @param dataset A sample dataset file.
#'
#' @export
get_sample_data <- function(dataset = "dfall.csv") {
  data_dir <- system.file("sample_data", package = "coveffectsplot")
  if (data_dir == "") {
    stop("Could not find data directory. Try re-installing `coveffectsplot`.",
         call. = FALSE)
  }

  data_files <- list.files(data_dir)
  if (!dataset %in% data_files) {
    stop("Data file does not exit. Possible files: ",
         paste(data_files, collapse = ", "),
         call. = FALSE)
  }
  utils::read.csv(file.path(data_dir, dataset))
}
