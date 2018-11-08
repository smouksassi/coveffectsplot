suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(egg) 
  library(ggplot2)
  library(ggstance)
  library(shiny)
  library(shinyjs) 
  library(markdown)
  library(colourpicker)
})

signif_pad <- function(x, digits = 3, round.integers = TRUE, round5up = TRUE) {
  eps <- ifelse(round5up, x*(10^(-(digits + 3))), 0)
  if (round.integers) {
    cx <- as.character(signif(x + eps, digits))  # Character representation of x
  } else {
    cx <- ifelse(x >= 10^digits,
                 as.character(round(x)),
                 as.character(signif(x + eps, digits)))  # Character representation of x
  }
  
  cx[is.na(x)] <- "0"                    # Put in a dummy value for missing x
  
  d <- gsub("[^0-9]", "", cx)            # The 'digits' of x
  d <- sub("^0*", "", d)                 # Remove any leading zeros
  nd <- nchar(d)                         # How many actual digits
  nd[cx == "0"] <- 1                     # Special case "0"
  npad <- pmax(0, digits - nd)           # How many digits are missing
  pad <- sapply(npad, function(n) paste(rep("0", times = n), collapse = ""))
  
  has.dec <- grepl("\\.", cx)                      #  Does cx already contain a decimal point?
  add.dec <- ifelse(!has.dec & npad > 0, ".", "")  #  If not, and if padding is required, we need to add a decimal point first
  
  ifelse(is.na(x), NA, paste(cx, add.dec, pad, sep = ""))
}
