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
          # it only makes sense if the number of sigma_e is the same as the
          # number of sigma
          if (n_sigma_e > 0 && n_sigma_e != n_sigma) {
            next
          }

          # sigma is represented as log sigma in the parameters
          params <- c(a[1:n_a], log(sigma[1:n_sigma]))

          if (n_sigma_e > 0) {
            # sigma_e is represented as the log of the multiplier of sigma
            # in the parameters
            params <- c(params, log(sigma_e[1:n_sigma_e] / sigma[1:n_sigma]))
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

    fit <- fit_data(data = data, jitter_settings = list(n = 0))

    expect_equal(
      fit$fitted_params$mu,
      later_mu,
      tolerance = 0.01
    )
    expect_equal(
      fit$fitted_params$sigma,
      later_sd,
      tolerance = 0.01
    )
  }
)


test_that(
  "LATER model fit (mu = 5, sigma = 1, criterion = 'likelihood')
  is as expected",
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

    fit <- fit_data(
      data = data,
      fit_criterion = "likelihood",
      jitter_settings = list(n = 0)
    )

    expect_equal(
      fit$fitted_params$mu,
      5.004784,
      tolerance = 0.01
    )
    expect_equal(
      fit$fitted_params$sigma,
      0.9956403,
      tolerance = 0.01
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

    fit <- fit_data(
      data = data,
      with_early_component = TRUE,
      jitter_settings = list(n = 0)
    )

    expect_equal(
      fit$fitted_params$mu,
      later_mu,
      tolerance = 0.01
    )
    expect_equal(
      fit$fitted_params$sigma,
      later_sd,
      tolerance = 0.05
    )
    expect_equal(
      fit$fitted_params$sigma_e,
      3.3,
      tolerance = 0.01
    )
  }
)


test_that(
  "LATER model fit (mu = 5, sigma = 0.5, sigma_e = 3,
  fit_criterion = 'likelihood') is as expected",
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

    fit <- fit_data(
      data = data,
      with_early_component = TRUE,
      fit_criterion = "likelihood",
      jitter_settings = list(n = 0)
    )

    expect_equal(
      fit$fitted_params$mu,
      later_mu,
      tolerance = 0.01
    )
    expect_equal(
      fit$fitted_params$sigma,
      later_sd,
      tolerance = 0.1
    )
    expect_equal(
      fit$fitted_params$sigma_e,
      3.3,
      tolerance = 0.01
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
      tolerance = 0.01
    )
  }
)


test_that(
  "Fitted KS values are similar to SPIC output at its fit values",
  {
    conditions <- c("p05", "p10", "p25", "p50", "p75", "p90", "p95")
    participants <- c("a", "b")

    spic_data <- data.frame(
      participant = rep(participants, times = rep(length(conditions), 2)),
      condition = rep(conditions, 2),
      ks = c(
        0.042,
        0.027,
        0.016,
        0.014,
        0.007,
        0.01,
        0.017,
        0.05,
        0.029,
        0.012,
        0.011,
        0.009,
        0.011,
        0.019
      ),
      mu = c(
        3.64,
        4.02,
        4.09,
        4.93,
        5.26,
        5.43,
        5.53,
        3.6,
        4,
        4.61,
        5.07,
        5.17,
        5.49,
        5.67
      ),
      sigma = c(
        0.61,
        0.73,
        0.67,
        0.74,
        0.78,
        0.77,
        0.85,
        0.7,
        0.76,
        0.94,
        1.04,
        1.06,
        1.16,
        1.18
      ),
      sigma_e = c(
        0.13,
        1.49,
        3.02,
        3.06,
        4.03,
        4.8,
        5.26,
        1.13,
        1.01,
        1.91,
        1.6,
        3.97,
        4.21,
        5.34
      )
    )

    fit_info <- list(
      n_a = 1,
      n_mu = 1,
      n_sigma = 1,
      n_sigma_e = 1,
      share_sigma = FALSE,
      share_sigma_e = FALSE,
      intercept_form = FALSE,
      use_minmax = FALSE,
      fit_criterion = "ks"
    )

    for (row in seq_len(nrow(spic_data))) {
      data <- (
        carpenter_williams_1995 |>
          dplyr::filter(
            .data$participant == spic_data[row, "participant"],
            .data$condition == spic_data[row, "condition"]
          ) |>
          dplyr::mutate(
            name = "ks_test",
            promptness = 1 / (.data$time / 1000),
            name_factor = factor(.data$name)
          ) |>
          dplyr::select(name, promptness, name_factor)
      )

      data <- add_ecdf_to_data(data = data)

      data <- set_data_param_indices(data = data, fit_info = fit_info)

      params <- c(
        spic_data[row, "mu"],
        log(spic_data[row, "sigma"]),
        log(spic_data[row, "sigma_e"] / spic_data[row, "sigma"])
      )

      curr_ks <- objective_function(
        params = params,
        data = data,
        fit_info = fit_info
      )

      expect_equal(
        round(curr_ks, 3),
        spic_data[row, "ks"],
        tolerance = 0.03
      )
    }
  }
)


test_that(
  "Two datasets without sharing raise an error",
  {
    n <- 500

    data_a <- data.frame(
      name = "a",
      promptness = 1 / simulate_dataset(
        n = n,
        later_mu = 5,
        later_sd = 1,
        early_sd = NULL,
        seed = 4685513
      )
    )

    data_b <- data.frame(
      name = "b",
      promptness = 1 / simulate_dataset(
        n = n,
        later_mu = 4,
        later_sd = 1.5,
        early_sd = NULL,
        seed = 6448
      )
    )

    data <- rbind(data_a, data_b)

    expect_error(
      fit_data(data = data, jitter_settings = list(n = 0))
    )

    expect_no_warning(
      fit_data(data = data, share_a = TRUE, jitter_settings = list(n = 0))
    )
  }
)


test_that(
  "AIC gives expected results",
  {
    # compared against Python statsmodels (0.14.0)
    expect_equal(
      calc_aic(loglike = -10000, n_params = 1),
      20002
    )

    expect_equal(
      calc_aic(loglike = -10000, n_params = 3),
      20006
    )

    expect_equal(
      calc_aic(loglike = -10000, n_params = 10),
      20020
    )

    expect_equal(
      calc_aic(loglike = -50000, n_params = 1),
      100002
    )

    expect_equal(
      calc_aic(loglike = -50000, n_params = 3),
      100006
    )

    expect_equal(
      calc_aic(loglike = -50000, n_params = 10),
      100020
    )
  }
)

test_that(
  "Fitting three datasets works as expected",
  {
    n <- 500

    data_a <- data.frame(
      name = "a",
      promptness = 1 / simulate_dataset(
        n = n,
        later_mu = 5,
        later_sd = 1,
        early_sd = NULL,
        seed = 489066781
      )
    )

    data_b <- data.frame(
      name = "b",
      promptness = 1 / simulate_dataset(
        n = n,
        later_mu = 5,
        later_sd = 1.5,
        early_sd = NULL,
        seed = 974149696
      )
    )

    data_c <- data.frame(
      name = "c",
      promptness = 1 / simulate_dataset(
        n = n,
        later_mu = 5,
        later_sd = 2.0,
        early_sd = NULL,
        seed = 279293069
      )
    )

    data <- rbind(data_a, data_b, data_c)

    fit <- fit_data(
      data = data,
      share_a = TRUE,
      jitter_settings = list(n = 0)
    )

    expect_equal(
      fit$fitted_params$mu,
      fit$fitted_params$mu[1]
    )

    expect_equal(
      fit$fitted_params$sigma,
      c(0.977, 1.512, 1.938),
      tolerance = 0.01
    )
  }
)

test_that(
  "Fitting with shared sigma and intercept form works",
  {
    n <- 500

    shared_sigma <- 2.0
    sigma_e <- NULL

    k_a <- 2.0
    k_b <- 3.0

    mu_a <- k_a * shared_sigma
    mu_b <- k_b * shared_sigma

    data_a <- data.frame(
      name = "a",
      promptness = 1 / simulate_dataset(
        n = n,
        later_mu = mu_a,
        later_sd = shared_sigma,
        early_sd = sigma_e,
        seed = 377877
      )
    )

    data_b <- data.frame(
      name = "b",
      promptness = 1 / simulate_dataset(
        n = n,
        later_mu = mu_b,
        later_sd = shared_sigma,
        early_sd = sigma_e,
        seed = 979696
      )
    )

    data <- rbind(data_a, data_b)

    fit <- fit_data(
      data = data,
      intercept_form = TRUE,
      share_sigma = TRUE,
      jitter_settings = list(n = 0)
    )

    expect_equal(
      fit$fitted_params$sigma,
      2.007,
      tolerance = 0.001
    )

    expect_equal(
      fit$fitted_params$k,
      c(1.967, 2.991),
      tolerance = 0.001
    )
  }
)


test_that(
  "Simulated data obeys the negative allowance option",
  {
    for (allow_negative_times in c(FALSE, TRUE)) {
      rt <- simulate_dataset(
        n = 100,
        later_mu = 1,
        later_sd = 1,
        early_sd = 1,
        seed = 1,
        allow_negative_times = allow_negative_times
      )

      has_negative_times <- sum(rt <= 0) > 0

      expect_equal(has_negative_times, allow_negative_times)
    }
  }
)


test_that(
  "Jitter parameter merging works",
  {
    default_n <- 7
    default_prop <- 0.5
    default_seed <- NA
    default_processes <- 2

    # no parameters passed
    expect_equal(
      merge_jitter_settings(list()),
      list(
        n = default_n,
        prop = default_prop,
        seed = default_seed,
        processes = default_processes
      )
    )

    # set n
    expect_equal(
      merge_jitter_settings(list(n = 2)),
      list(
           n = 2,
           prop = default_prop,
           seed = default_seed,
           processes=default_processes
      )
    )

    # set prop
    # the sort business is because `expect_equal` seems to compare name order
    expect_equal(
      sort(unlist(merge_jitter_settings(list(prop = 0.1)))),
      sort(unlist(list(n = default_n, prop = 0.1, seed = default_seed, processes=default_processes)))
    )

    # set seed
    expect_equal(
      sort(unlist(merge_jitter_settings(list(seed = 123)))),
      sort(unlist(list(n = default_n, prop = default_prop, seed = 123, processes=default_processes)))
    )

    # set processes
    expect_equal(
      sort(unlist(merge_jitter_settings(list(processes = 1)))),
      sort(
        unlist(
          list(
            n = default_n,
            prop = default_prop,
            seed = default_seed,
            processes = 1
          )
        )
      )
    )
  }
)

test_that(
  "jittering works as expected",
  {
    # simulate a dataset to use
    promptness <- 1 / simulate_dataset(
      n = 100,
      later_mu = 5,
      later_sd = 2.0,
      early_sd = NULL,
      seed = 124124
    )
    data <- data.frame(name = "test", promptness = promptness)

    # no jittering if asked
    fit <- fit_data(data = data, jitter_settings = list(n = 0))

    expect_equal(length(fit$jitter_optim_results), 1)

    # doing it again should give the same result
    expect_equal(fit, fit_data(data = data, jitter_settings = list(n = 0)))

    # jitter
    fit <- fit_data(data = data, jitter_settings = list(n = 2))

    expect_equal(length(fit$jitter_optim_results), 3)

    # seed check
    seed <- 12523154
    fit_1 <- fit_data(data = data, jitter_settings = list(n = 2, seed = seed))
    fit_2 <- fit_data(data = data, jitter_settings = list(n = 2, seed = seed))

    expect_equal(fit_1, fit_2)

    # unseeded
    fit_1 <- fit_data(data = data, jitter_settings = list(n = 2))
    fit_2 <- fit_data(data = data, jitter_settings = list(n = 2))

    # jitter amounts should be different
    expect_true(fit_1$jitters[[2]][[1]] != fit_2$jitters[[2]][[1]])
    expect_true(fit_1$jitters[[2]][[2]] != fit_2$jitters[[2]][[2]])
  }
)
