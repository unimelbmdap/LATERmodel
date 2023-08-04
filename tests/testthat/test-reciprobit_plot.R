test_that("Plotting in reciprobit space works as expected", {
  #TODO: look into https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#testing-ggplot2-output
  disp_reciprobit <- reciprobit_plot(rt)
  vdiffr::expect_doppelganger("Plot in reciprobit space", disp_reciprobit)
})
