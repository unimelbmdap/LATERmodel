test_that("Plot one dataset in reciprobit space", {
  #TODO: look into https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#testing-ggplot2-output
  df <- prepare_data(rt)
  disp_reciprobit <- reciprobit_plot(df)
  vdiffr::expect_doppelganger("One dataset in reciprobit space", disp_reciprobit)
})

test_that("Plot two datasets in reciprobit space", {
  #TODO: look into https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#testing-ggplot2-output
  df <- prepare_data(rt2)
  disp_reciprobit <- reciprobit_plot(df)
  vdiffr::expect_doppelganger("Two datasets in reciprobit space", disp_reciprobit)
})
