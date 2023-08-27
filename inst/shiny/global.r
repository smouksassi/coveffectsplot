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
  library(grid)
  library(shinymeta)
  library(glue)
  library(rlang)
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

###from scales
label_wrap <- function(width) {
  force(width)
  function(x) {
    unlist(lapply(strwrap(x, width = width, simplify = FALSE), 
                  paste0, collapse = "\n"))
  }
}
###from scales
pch_table <- c(`square open` = 0, `circle open` = 1, `triangle open` = 2, 
               plus = 3, cross = 4, `diamond open` = 5, `triangle down open` = 6, 
               `square cross` = 7, asterisk = 8, `diamond plus` = 9, 
               `circle plus` = 10, star = 11, `square plus` = 12, `circle cross` = 13, 
               `square triangle` = 14, `triangle square` = 14, square = 15, 
               `circle small` = 16, triangle = 17, diamond = 18, circle = 19, 
               bullet = 20, `circle filled` = 21, `square filled` = 22, 
               `diamond filled` = 23, `triangle filled` = 24, `triangle down filled` = 25)
