suppressPackageStartupMessages({
  library(coveffectsplot)
  library(dplyr)
  library(tidyr)
  library(egg)
  library(ggplot2)
  library(ggstance)
  library(shiny)
  library(shinyjs)
  library(markdown)
  library(colourpicker)
  library(shinymeta)
})

escape_newline <- function(s) {
  gsub("\\\\n", "\\\n", s)
}

round_pad <- function(x, digits = 2, round5up = TRUE) {
  eps <- if (round5up) x * (10^(-(digits + 3))) else 0
  formatC(round(x + eps, digits), digits = digits, format = "f", flag = "0")
}
