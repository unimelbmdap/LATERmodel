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
