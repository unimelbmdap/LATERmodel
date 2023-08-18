
fit_data <- function(data) {

  if(is.vector(data)) {
    data = data.frame(
      times = data,
      i_mu = rep(1, length(data)),
      i_sigma = rep(1, length(data))
    )
  }

  data$promptness = 1.0 / data$times

  check_data(data)

  i_mu_factor = factor(data$i_mu)
  i_sigma_factor = factor(data$i_sigma)

  # how many unique mu and sigma parameters there are
  n_mu = length(levels(i_mu_factor))
  n_sigma = length(levels(i_sigma_factor))

  start_points = calc_start_points(data)

  fit = optim(
    start_points$value,
    objective_function,
    data = data,
    n_mu = n_mu,
    n_sigma = n_sigma
  )

  return(fit)

}

objective_function <- function(params, data, n_mu, n_sigma) {

  mu = params[1:n_mu]
  sigma = params[(n_mu + 1):length(params)]

  p = pnorm(
    data$promptness,
    mu[data$i_mu],
    sigma[data$i_sigma]
  )

  ecdf_function = ecdf(data$promptness)
  ecdf_p = ecdf_function(data$promptness)

  ks = calc_ks_stat(ecdf_p, p)

}


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

  start_points = list(
    value = c(mu_values, sigma_values),
    param = c(rep("MU", length(mu_values)), rep("SIGMA", length(sigma_values)))
  )

}


check_data <- function(data) {

  mu_factor = factor(data$mu)

}

calc_ks_stat <- function(ecdf_p, cdf_p) {
  max(abs(ecdf_p - cdf_p))
}
