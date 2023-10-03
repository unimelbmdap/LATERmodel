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
  "parameter unpacking works",
  {

    # some dummy values for the parameters
    a <- c(1, 2)
    sigma <- c(3, 4)
    sigma_e <- c(5, 6)

    for (n_a in c(1, 2)) {
      for (n_sigma in c(1, 2)) {
        for (n_sigma_e in c(0, 1, 2)) {

          params <- c(a[1:n_a], sigma[1:n_sigma])

          if (n_sigma_e > 0) {
            params <- c(params, sigma_e[1:n_sigma_e])
          }

          unpacked <- unpack_params(
            params = params,
            n_a = n_a,
            n_sigma = n_sigma,
            n_sigma_e = n_sigma_e
          )

          expected <- list(
            a = a[1:n_a],
            sigma = sigma[1:n_sigma]
          )

          if (n_sigma_e > 0) {
            expected$sigma_e <- sigma_e[1:n_sigma_e]
          }

          expect_equal(unpacked, expected)

        }

      }

    }

  }
)


test_that(
  "LATER model fit (mu = 5, sigma = 1) is as expected",
  {

    seed <- 23256312

    # Carpenter & Noorani (2023), Figure 1.15
    later_mu <- 5
    later_sd <- 1
    early_sd <- NULL

    n <- 5000

    times <- simulate_dataset(
      n = n,
      later_mu = later_mu,
      later_sd = later_sd,
      early_sd = early_sd,
      seed = seed
    )

    data <- data.frame(name = "test", promptness = 1 / times)

    fit <- fit_data(data = data)

    expect_equal(
      fit$fitted_params$mu,
      5.004784,
      tolerance = 6
    )
    expect_equal(
      fit$fitted_params$sigma,
      0.9956403,
      tolerance = 6
    )

  }
)


test_that(
  "LATER model fit (mu = 5, sigma = 0.5, sigma_e = 3) is as expected",
  {

    seed <- 946395130

    # Carpenter & Noorani (2023), Figure 2.5
    later_mu <- 5
    later_sd <- 0.5
    early_sd <- 3

    n <- 1000

    times <- simulate_dataset(
      n = n,
      later_mu = later_mu,
      later_sd = later_sd,
      early_sd = early_sd,
      seed = seed
    )

    data <- data.frame(name = "test", promptness = 1 / times)

    fit <- fit_data(data = data, with_early_component = TRUE)

    expect_equal(
      fit$fitted_params$mu,
      5.01478,
      tolerance = 6
    )
    expect_equal(
      fit$fitted_params$sigma,
      0.5054952,
      tolerance = 6
    )
    expect_equal(
      fit$fitted_params$sigma_e,
      3.156108,
      tolerance = 6
    )

  }
)


test_that(
  "ECDF matches the application for C&W1995 (A, p50)",
  {

    # copied from the application log for this dataset
    application_pcnt <- c(
      0.29,
      0.59,
      0.73,
      0.95,
      1.54,
      2.78,
      6.15,
      12.82,
      21.47,
      35.02,
      49.96,
      60,
      71.14,
      79.41,
      87.11,
      91.36,
      93.04,
      94.65,
      96.78,
      97.73,
      98.53,
      99.27,
      99.49,
      99.93,
      100
    )

    times <- carpenter_williams_1995 |>
      dplyr::filter(
        .data$participant == "a",
        .data$condition == "p50",
      ) |>
      dplyr::pull("time")

    promptness <- 1 / (times / 1000)

    p <- promptness_ecdf(promptness = promptness, eval_unique = TRUE)
    pcnt <- p$y * 100

    expect_equal(
      100 - pcnt,
      application_pcnt,
      tolerance = 2
    )

  }
)
