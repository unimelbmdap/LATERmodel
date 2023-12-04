## code to prepare `reddi_asrress_carpenter_2003` dataset goes here

#reddi_asrress_carpenter_2003 <- read.csv("data-raw/Reddi_Asrress_Carpenter_2003_extrapolated.csv")
reddi_asrress_carpenter_2003 <- read.csv("Reddi_Asrress_Carpenter_2003_extrapolated.csv")

usethis::use_data(reddi_asrress_carpenter_2003, overwrite = TRUE)
