## code to prepare `DATASET` dataset goes here
usethis::use_data_raw()
prezista <- readr::read_csv("./data-raw/druglabel.csv")
usethis::use_data(prezista, overwrite = TRUE)
wtage <- readr::read_csv("./data-raw/wtage.csv")
usethis::use_data(wtage, overwrite = TRUE)