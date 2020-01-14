usethis::use_data_raw()
prezista <- readr::read_csv("./data-raw/druglabel.csv")

usethis::use_data(prezista, overwrite = TRUE)