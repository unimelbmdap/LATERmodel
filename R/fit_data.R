
load_demo_data <- function() {

  raw <- read.csv(
    "../../demo_data/spic_demo_mu4_sigma1.SAS",
    sep = "\t",
    header = FALSE
  )
  rt <- (raw[, 5] * 10) / 1000
  return(rt)

}

load_demo_pair_data <- function(share_mu = FALSE, share_sigma = FALSE) {

  raw_1 <- read.csv(
    "../../demo_data/spic_demo_mu4_sigma1.SAS",
    sep = "\t",
    header = FALSE
  )
  rt_1 <- (raw_1[, 5] * 10) / 1000

  raw_2 <- read.csv(
    "../../demo_data/spic_demo_mu4_sigma0_5.SAS",
    sep = "\t",
    header = FALSE
  )
  rt_2 <- (raw_2[, 5] * 10) / 1000

  data <- data.frame(
    times = c(rt_1, rt_2),
    dataset = c(rep(1, length(rt_1)), rep(2, length(rt_2))),
    i_mu = rep(1, length(rt_1) + length(rt_2)),
    i_sigma = rep(1, length(rt_1) + length(rt_2))
  )

  if (!share_mu) {
    data$i_mu[(length(rt_1) + 1):nrow(data)] <- 2
  }

  if (!share_sigma) {
    data$i_sigma[(length(rt_1) + 1):nrow(data)] <- 2
  }

  return(data)

}

fit_data <- function(data, use_minmax = FALSE) {

  # if `data` is just a vector of RTs, assume a two-parameter model
  # and construct an appropriate data frame
  if (is.vector(data)) {
    data <- data.frame(
      times = data,
      i_mu = rep(1, length(data)),
      i_sigma = rep(1, length(data)),
      dataset = rep(1, length(data))
    )
  }

  data$promptness <- 1.0 / data$times

  check_data(data)

  # how many unique mu and sigma parameters there are
  n_mu <- length(unique(data$i_mu))
  n_sigma <- length(unique(data$i_sigma))

  start_points <- calc_start_points(data)

  fit <- optim(
    start_points,
    objective_function,
    data = data,
    n_mu = n_mu,
    n_sigma = n_sigma,
    use_minmax = use_minmax
  )

  return(fit)

}


# returns the KS statistic given a set of model parameter values
# and the observed data
objective_function <- function(params, data, n_mu, n_sigma, use_minmax) {

  # first `n_mu` items are the mu parameters
  mu <- params[1:n_mu]
  # drop the `mu` parameters to get the sigma parameters
  sigma <- params[-(1:n_mu)]

  # calculate the expected cumulative probability of each data point
  # under the model
  ks <- data |>
    dplyr::group_by(.data$dataset) |>
    dplyr::mutate(
      p = pnorm(
        .data$promptness,
        mu[.data$i_mu],
        sigma[.data$i_sigma]
      ),
      ecdf_p = stats::ecdf(.data$promptness)(.data$promptness)
    ) |>
    dplyr::summarize(
      ks = calc_ks_stat(.data$ecdf_p, .data$p)
    ) |>
    dplyr::pull(ks)

  if (use_minmax) {
    # "minimise the worst of the fits"
    ks <- max(ks)
  } else {
    # "minimise the overall goodness-of-fit statistic"
    ks <- sum(ks)
  }

  return(ks)

}


# use the sample mean and standard deviation of the promptness values
# to create optimisation starting points
calc_start_points <- function(data) {

  mu_values <- (
    data |>
      dplyr::group_by(.data$i_mu) |>
      dplyr::summarize(val = mean(.data$promptness)) |>
      dplyr::pull(.data$val)
  )

  sigma_values <- (
    data |>
      dplyr::group_by(.data$i_sigma) |>
      dplyr::summarize(val = sd(.data$promptness)) |>
      dplyr::pull(.data$val)
  )

  start_points <- c(mu_values, sigma_values)

  return(start_points)

}

# TODO: add checks that the data is as expected
check_data <- function(data) {

}


# calculates the Kolmogorov-Smirnov statistic
calc_ks_stat <- function(ecdf_p, cdf_p) {

  max(abs(ecdf_p - cdf_p))

}

# evaluates the cumulative density distribution when there are both early
# and late components and the draw is given by the maximum of the two
pnorm_with_early <- function(q, later_mu, later_sd, early_mu, early_sd) {

  # cdf of the maximum of two independent gaussians is the product of
  # their individual values
  early_p <- pnorm(q, early_mu, early_sd)
  later_p <- pnorm(q, later_mu, later_sd)

  p <- early_p * later_p

  return(p)

}


# evaluates the probability density function when there are both early
# and late components and the draw is given by the maximum of the two
dnorm_with_early <- function(x, later_mu, later_sd, early_mu, early_sd) {

  return(
    (
      (
        exp(-(((x - later_mu) ** 2) / (2 * later_sd ** 2)))
        * (1 + erf((x - early_mu) / (sqrt(2) * early_sd)))
      ) / later_sd
      + (
        exp(-(((x - early_mu) ** 2) / (2 * early_sd ** 2)))
        * (1 + erf((x - later_mu) / (sqrt(2) * later_sd)))
      ) / early_sd
    ) / (2 * sqrt(2 * pi))
  )

}

erf <- function(x) {

  return(2 * pnorm(x * sqrt(2)) - 1)

}
