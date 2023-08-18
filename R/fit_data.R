
fit_data <- function(data, use_minmax = FALSE) {

  # if `data` is just a vector of RTs, assume a two-parameter model and construct
  # an appropriate data frame
  if(is.vector(data)) {
    data = data.frame(
      times = data,
      i_mu = rep(1, length(data)),
      i_sigma = rep(1, length(data)),
      dataset = rep(1, length(data))
    )
  }

  data$promptness = 1.0 / data$times

  check_data(data)

  # how many unique mu and sigma parameters there are
  n_mu = length(unique(data$i_mu))
  n_sigma = length(unique(data$i_sigma))

  start_points = calc_start_points(data)

  fit = optim(
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
  mu = params[1:n_mu]
  # drop the `mu` parameters to get the sigma parameters
  sigma = params[-(1:n_mu)]

  # calculate the expected cumulative probability of each data point under the model
  p = pnorm(
    data$promptness,
    mu[data$i_mu],
    sigma[data$i_sigma]
  )

  if(use_minmax) {

    ks = data |>
      dplyr::mutate(p = p) |>
      dplyr::group_by(.data$dataset) |>
      dplyr::summarize(
        ks = calc_ks_stat(
          ecdf(.data$promptness)(.data$promptness),
          .data$p
        )
      ) |>
      dplyr::pull(ks)

    # "minimise the worst of the fits"
    ks = max(ks)

  } else {

    # calculate the empirical cumulative probability of each data point
    ecdf_function = ecdf(data$promptness)
    ecdf_p = ecdf_function(data$promptness)

    ks = calc_ks_stat(ecdf_p, p)
  }

  return(ks)

}


# use the sample mean and standard deviation of the promptness values
# to create optimisation starting points
calc_start_points <- function(data) {

  mu_values = (
    data |>
    dplyr::group_by(.data$i_mu) |>
    dplyr::summarize(val = mean(.data$promptness)) |>
    dplyr::pull(val)
  )

  sigma_values = (
    data |>
    dplyr::group_by(.data$i_sigma) |>
    dplyr::summarize(val = sd(.data$promptness)) |>
    dplyr::pull(val)
  )

  start_points = c(mu_values, sigma_values)

}

# TODO: add checks that the data is as expected
check_data <- function(data) {
  #mu_factor = factor(data$mu)
}


# calculates the Kolmogorov-Smirnov statistic
calc_ks_stat <- function(ecdf_p, cdf_p) {
  max(abs(ecdf_p - cdf_p))
}
