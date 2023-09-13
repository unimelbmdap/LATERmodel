
load_carpenter_data <- function() {
  raw <- read.csv("../../demo_data/Carpenter_Williams_Nature_1995.csv")
}


get_example_data <- function(showing) {

  data <- load_carpenter_data()

  if (showing == "swivel") {
    data <- data |>
      dplyr::filter(.data$Condition %in% c("p05", "p95")) |>
      dplyr::mutate(
        name = .data$Condition,
        promptness = 1 / (.data$Times / 1000)
      )
  }
  else {
    stopifnot(FALSE)
  }

  return(data)

}


fit_data_model <- function(
  data,
  share_a = FALSE,
  share_sigma = FALSE,
  share_sigma_e = FALSE,
  with_early_component = FALSE,
  intercept_form = FALSE,
  use_minmax = FALSE
) {

  name_factor <- factor(data$name)

  n_names <- length(levels(name_factor))

  stopifnot(n_names %in% c(1, 2))

  fit_info <- list(
    levels = levels(name_factor),
    n_a = as.numeric(!share_a) + (n_names - 1),
    n_sigma = as.numeric(!share_sigma) + (n_names - 1),
    n_sigma_e = (as.numeric(!share_sigma_e) + (n_names - 1)) * as.numeric(with_early_component),
    with_early_component = with_early_component,
    intercept_form = intercept_form,
    use_minmax = use_minmax,
    fit = NA
  )

  if (!intercept_form) {
    fit_info$n_mu = fit_info$n_a
  }
  else {
    if (fit_info$n_sigma == 1) {
      fit_info$n_mu = fit_info$n_a
    }
    else {
      fit_info$n_mu = 2
    }
  }

  data <- data.frame(
    name = data$name,
    promptness = data$promptness
  )
  
  data$ecdf_p <- stats::ecdf(data$promptness)(data$promptness)

  i_name <- as.integer(name_factor)

  if (fit_info$n_mu == 1) {
    data$i_mu <- 1
  }
  else {
    data$i_mu <- i_name
  }

  if (share_sigma) {
    data$i_sigma <- 1
  }
  else {
    data$i_sigma <- i_name
  }

  if (share_sigma_e) {
    data$i_sigma_e <- 1
  }
  else {
    data$i_sigma_e <- i_name
  }

  fit_info$start_points <- calc_start_points(data = data, fit_info = fit_info)

  fit <- optim(
    fit_info$start_points,
    objective_function,
    data = data,
    fit_info = fit_info,
  )

  fit_info$fit = fit

  fit_info$fitted_params = unpack_params(
    params = fit$par,
    n_a = fit_info$n_a,
    n_sigma = fit_info$n_sigma,
    n_sigma_e = fit_info$n_sigma_e
  )

  fit_info$fitted_params <- append(
    fit_info$fitted_params,
    convert_a_to_mu_and_k(
      a = fit_info$fitted_params$a,
      sigma = fit_info$fitted_params$sigma,
      intercept_form = fit_info$intercept_form
    )
  )

  fit_info$fitted_params$s = 1 / fit_info$fitted_params$sigma

  return(fit_info)

}

unpack_params <- function(params, n_a, n_sigma, n_sigma_e) {

  # first `n_a` items are the a parameters
  a <- params[1:n_a]
  # next are the sigma parameters
  sigma <- params[(n_a + 1):(n_a + n_sigma)]

  labelled_params <- list(a = a, sigma = sigma)

  if (n_sigma_e > 0) {
    sigma_e <- params[(n_a + n_sigma + 1):length(params)]
    labelled_params$sigma_e <- sigma_e
  }

  return(labelled_params)

}

convert_a_to_mu_and_k <- function(a, sigma, intercept_form) {

  if (intercept_form) {
    k <- a
    mu <- k * sigma
  }
  else {
    mu <- a
    k = mu / sigma
  }

  return(list(mu=mu, k=k))

}

# returns the KS statistic given a set of model parameter values
# and the observed data
objective_function <- function(params, data, fit_info) {

  labelled_params <- unpack_params(
    params = params,
    n_a = fit_info$n_a,
    n_sigma = fit_info$n_sigma,
    n_sigma_e = fit_info$n_sigma_e
  )

  labelled_params <- append(
    labelled_params,
    convert_a_to_mu_and_k(
      a = labelled_params$a,
      sigma = labelled_params$sigma,
      intercept_form = fit_info$intercept_form
      )
  )

  # calculate the expected cumulative probability of each data point
  # under the model
  ks <- data |>
    dplyr::group_by(.data$name) |>
    dplyr::mutate(
      p = dplyr::case_when(
        fit_info$with_early_component ~ pnorm_with_early(
          .data$promptness,
          labelled_params$mu[.data$i_mu],
          labelled_params$sigma[.data$i_sigma],
          labelled_params$sigma_e[.data$i_sigma_e]
        ),
        !fit_info$with_early_component ~ pnorm(
          .data$promptness,
          labelled_params$mu[.data$i_mu],
          labelled_params$sigma[.data$i_sigma]
        )
      ),
      ecdf_p = stats::ecdf(.data$promptness)(.data$promptness)
    ) |>
    dplyr::summarize(
      ks = calc_ks_stat(.data$ecdf_p, .data$p)
    ) |>
    dplyr::pull(ks)

  if (fit_info$use_minmax) {
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
calc_start_points <- function(data, fit_info) {

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

  if (fit_info$intercept_form) {
    a_values <- mu_values / sigma_values
    if (fit_info$n_a == 1) {
      a_values <- mean(a_values)
    }
  }
  else {
    a_values <- mu_values
  }

  start_points <- c(a_values, sigma_values)

  if (fit_info$with_early_component) {
    sigma_e_values <- (
      data |>
        dplyr::group_by(.data$i_sigma_e) |>
        dplyr::summarize(val = sd(.data$promptness) * 3) |>
        dplyr::pull(.data$val)
    )

    start_points <- c(start_points, sigma_e_values)
  }

  return(start_points)

}

# calculates the Kolmogorov-Smirnov statistic
calc_ks_stat <- function(ecdf_p, cdf_p) {
  max(abs(ecdf_p - cdf_p))
}

# evaluates the cumulative density distribution when there are both early
# and late components and the draw is given by the maximum of the two
pnorm_with_early <- function(q, later_mu, later_sd, early_sd) {

  early_mu <- 0

  # constrain the SDs to be > 0
  early_sd <- max(early_sd, 1e-5)
  later_sd <- max(later_sd, 1e-5)

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
