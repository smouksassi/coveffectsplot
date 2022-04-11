## code to prepare `DATASET` dataset goes here
usethis::use_data_raw()

prezista <- readr::read_csv("./data-raw/druglabel.csv")
usethis::use_data(prezista, overwrite = TRUE)

wtage <- readr::read_csv("./data-raw/wtage.csv")
usethis::use_data(wtage, overwrite = TRUE)

covdatasim <- readr::read_csv("./data-raw/covdatasim.csv")
usethis::use_data(covdatasim, overwrite = TRUE)


covdatasim$SEX<- ifelse(covdatasim$SEX==0,1,0)
covdatasim$SEX <- as.factor(covdatasim$SEX )
covdatasim$SEX <- factor(covdatasim$SEX,labels = c("Female","Male"))