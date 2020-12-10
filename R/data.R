#' Prezista Drug Label Data
#'
#' A dataset containing an excerpt from the official Prezista FDA Drug Label
#'  to help in the app exploration.
#'
#' @format A dataset with 33 rows and 6 variables
#' \describe{
#'   \item{covname}{Covariate Name, a character variable with two values
#'    Protease Inihibitors and Other Antiretrovirals}
#'   \item{label}{Covariate value label, a character variable with several possible values}
#'   \item{paramname}{Parameter on which the effects are shown,
#'    a character variable with three possible values
#'    Cmax, AUC and Cmin}
#'   \item{mid}{Middle value for the effects, the median from the uncertainty distribution}
#'   \item{lower}{Lower value for the effects usually the 5\% from the uncertainty distribution}
#'   \item{upper}{Upper value for the effects usually the 95\% from the uncertainty distribution}
#' }
#' @source Table 16 from \url{https://www.accessdata.fda.gov/drugsatfda_docs/label/2017/021976s045_202895s020lbl.pdf}
"prezista"

