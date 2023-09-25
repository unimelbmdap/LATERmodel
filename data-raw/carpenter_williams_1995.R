## code to prepare `carpenter_williams_1995` dataset goes here

carpenter_williams_1995 <- read.csv("../../The University of Melbourne/mdap - Documents/04 Collaborations/2023_Collaborations/05_MDHS_Anderson_LATER/Data/Carpenter_Williams_Nature_1995.csv")

usethis::use_data(carpenter_williams_1995, overwrite = TRUE)
