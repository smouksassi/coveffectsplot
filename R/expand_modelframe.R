#' Expand covariate values choices and reference values varying one at a time
#' 
#' @param rv  a data.frame with columns names of covariate(s) and values equal reference
#' @param covcol column name for the covariate being varied
#' @param ... 	Arguments to be passed to methods
#' @return A data.frame with combination of covariates
#' @examples
#' reference.values <- data.frame(WT = 85, ALB = 45, SEX = 0)   
#' covcomb <- expand_modelframe(
#'   WT  = c(56, 72, 98, 128), # P05, P25, P75, P95 # ref is P50
#' ALB = c(40, 50),          # P05, P95 # ref is P50
#'   SEX = c(1),               # Reference is for SEX=0 (female)
#'   rv = reference.values)
#' covcomb
#' @export
expand_modelframe <- function( rv, covcol="covname", ...) {
  args <- list(...)
  df <- lapply(args, function(x) x[[1]])
  df[names(rv)] <- rv
  res <- lapply(seq_along(rv), function(i) {
    df[[covcol]] <- names(rv)[i]
    df[[names(rv)[i]]] <- args[[names(rv)[i]]]
    as.data.frame(df)
  })
  do.call(rbind, res)
}



