test_that(
  "erf is as expected",
  {
    x <- c(-2, -1, 0, 1, 2)
    # compare against `scipy.special.erf`
    expect_equal(
      erf(x = x),
      c(-0.99532227, -0.84270079, 0, 0.84270079, 0.99532227)
    )
  }
)


test_that(
  "LATER model fit (mu = 3, sigma = 1) is as expected",
  {

    seed = 23256312

    later_mu = 3
    later_sd = 1
    early_sd = NULL

    n = 10000

    times = simulate_dataset(
      n = n,
      later_mu = later_mu,
      later_sd = later_sd,
      early_sd = early_sd,
      seed = seed
    )

    data = data.frame(name = "test", promptness = 1 / times)

    fit = fit_data(data = data)

    expect_equal(fit$fitted_params$mu, 2.991689, tolerance = 6)
    expect_equal(fit$fitted_params$sigma, 0.9914882, tolerance = 6)

  }
)


test_that(
  "LATER model fit (mu = 3, sigma = 1, sigma_e = 5) is as expected",
  {

    seed = 946395130

    later_mu = 3
    later_sd = 1
    early_sd = 5

    n = 10000

    times = simulate_dataset(
      n = n,
      later_mu = later_mu,
      later_sd = later_sd,
      early_sd = early_sd,
      seed = seed
    )

    data = data.frame(name = "test", promptness = 1 / times)

    fit = fit_data(data = data, with_early_component = TRUE)

    expect_equal(fit$fitted_params$mu, 3.552097, tolerance = 6)
    expect_equal(fit$fitted_params$sigma, 1.303464, tolerance = 6)
    expect_equal(fit$fitted_params$sigma_e, 6.529583, tolerance = 6)

  }
)
