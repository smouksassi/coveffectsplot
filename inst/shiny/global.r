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
###from table1
signif_pad <- function (x, digits = 3, round.integers = TRUE, round5up = TRUE) 
{
  eps <- if (round5up) 
    x * (10^(-(digits + 3)))
  else 0
  if (round.integers) {
    cx <- as.character(signif(x + eps, digits))
  }
  else {
    cx <- ifelse(x >= 10^digits, as.character(round(x)), 
                 as.character(signif(x + eps, digits)))
  }
  cx[is.na(x)] <- "0"
  d <- gsub("[^0-9]", "", cx)
  d <- sub("^0*", "", d)
  nd <- nchar(d)
  nd[cx == "0"] <- 1
  npad <- pmax(0, digits - nd)
  pad <- sapply(npad, function(n) paste(rep("0", times = n), 
                                        collapse = ""))
  has.dec <- grepl("\\.", cx)
  add.dec <- ifelse(!has.dec & npad > 0, ".", "")
  ifelse(is.na(x), NA, paste(cx, add.dec, pad, sep = ""))
}
###from table1
