## code to prepare `carpenter_williams_1995` dataset goes here

carpenter_williams_1995 <- read.csv("Carpenter_Williams_Nature_1995.csv")

usethis::use_data(carpenter_williams_1995, overwrite = TRUE)
