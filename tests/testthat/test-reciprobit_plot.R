test_that("Plot one dataset in reciprobit space", {
  # TODO: look into https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#testing-ggplot2-output
  df <- prepare_data(rt)
  disp_reciprobit <- reciprobit_plot(df)
  vdiffr::expect_doppelganger("One dataset in reciprobit space", disp_reciprobit)
})

test_that("Plot two datasets in reciprobit space", {
  df <- prepare_data(rt2)
  disp_reciprobit <- reciprobit_plot(df)
  vdiffr::expect_doppelganger("Two datasets in reciprobit space", disp_reciprobit)
})

test_that("Plot Carpenter and Williams (1995) Fig1a in reciprobit space", {
  df <- prepare_data(dplyr::filter(carpenter_williams_1995, participant == "a"))
  disp_reciprobit <- reciprobit_plot(df)
  vdiffr::expect_doppelganger("C&W(1995)Fig1a", disp_reciprobit)
})

test_that("Plot Carpenter and Williams (1995) Fig1b in reciprobit space", {
  df <- prepare_data(dplyr::filter(carpenter_williams_1995, participant == "b"))
  disp_reciprobit <- reciprobit_plot(df)
  vdiffr::expect_doppelganger("C&W(1995)Fig1b", disp_reciprobit)
})
