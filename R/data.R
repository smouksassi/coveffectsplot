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


#' Weight Age CDC growth charts data
#'
#' Weight-for-age, 2 to 20 years, LMS parameters and selected smoothed weight percentiles in kilograms, by sex and age.
#'
#' @format A dataset with 436 rows and 14 variables
#' \describe{
#'   \item{Sex}{1=male; 2=female}
#'   \item{Agemos}{Age in months}
#'   \item{L}{skewness ditribution parameter}
#'   \item{M}{location ditribution parameter}
#'   \item{S}{scale ditribution parameter}
#'   \item{P3}{Smoothed third percentile}
#'   \item{P5}{Smoothed fifth percentile}
#'   \item{P10}{Smoothed tenth percentile}
#'   \item{P25}{Smoothed twenty fifth percentile}
#'   \item{P50}{Smoothed fiftieth percentile}
#'   \item{P75}{Smoothed seventy fifth percentile}
#'   \item{P90}{Smoothed ninetieth percentile}
#'   \item{P95}{Smoothed ninety fifth percentile}
#'   \item{P97}{Smoothed ninety seventh percentile}
#' }
#' @source CDC website \url{https://www.cdc.gov/growthcharts/data/zscore/wtage.csv}
"wtage"

#' Correlated Covariates data
#'
#' A example dataset used to illustrate multivariate joint covariate effects.
#'
#' @format A dataset with 2000 rows and 5 variables
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{AGE}{Age in years}
#'   \item{WT}{Weight in kg}
#'   \item{Sex}{0=male; 1=female}
#'   \item{ALB}{Albumin in g/dL}
#' }
#' @source simulated based on a real dataset
"covdatasim"
