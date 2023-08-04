## code to prepare `rt` dataset goes here
test_data <- dplyr::tribble(
~rt, ~raw,
170, 8,
180, 19,
190, 37,
200, 43,
210, 35,
220, 19,
230, 19,
240, 5,
250, 6,
260, 6,
270, 2,
300, 1
)

rt <- rep(test_data$rt, times=test_data$raw)

usethis::use_data(rt, overwrite = TRUE)
