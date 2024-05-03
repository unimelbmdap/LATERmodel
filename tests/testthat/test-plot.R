test_that("Carpenter and Williams (1995) Fig1a individual fits", {
  df <- prepare_data(dplyr::filter(carpenter_williams_1995, participant == "a"))
  fit_params <- individual_later_fit(
    df,
    with_early_component = TRUE,
    fit_criterion = "likelihood",
    jitter_settings = list(n = 0)
  )
  disp_reciprobit <- reciprobit_plot(df, fit_params)
  vdiffr::expect_doppelganger("C&W(1995)Fig1a", disp_reciprobit)
})

test_that("Carpenter and Williams (1995) Fig1b shared intercept", {
  df <- prepare_data(dplyr::filter(carpenter_williams_1995, participant == "b"))
  fit_params <- fit_data(
    df,
    with_early_component = TRUE,
    share_a = TRUE,
    intercept_form = TRUE,
    jitter_settings = list(n = 0)
  )
  disp_reciprobit <- reciprobit_plot(df, fit_params$named_fit_params)
  vdiffr::expect_doppelganger("C&W(1995)Fig1b", disp_reciprobit)
})

test_that("Reddi et. al (2003) Fig2JS shared sigma", {
  df <- prepare_data(reddi_asrress_carpenter_2003)
  fit_params <- fit_data(df, share_sigma = TRUE, jitter_settings = list(n = 0))
  disp_reciprobit <- reciprobit_plot(df, fit_params$named_fit_params)
  skip_on_os("mac")
  vdiffr::expect_doppelganger("RAC(2003)Fig2JS", disp_reciprobit)
})
