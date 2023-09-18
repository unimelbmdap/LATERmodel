#' Fit a LATER model to a single dataset or a pair of datasets.
#' 
#' @param data A data frame with columns `name` and `promptness`.
#' @param share_a,share_sigma,share_sigma_e If `FALSE` (the default), each
#'  dataset has its own parameter. If `TRUE`, the datasets share the relevant
#'  parameter.
#' @param with_early_component If `TRUE`, the model contains a second 'early'
#'  component that is absent when `FALSE` (the default).
#' @param intercept_form If `FALSE` (the default), the `a` parameter describes
#'  the mu parameter in the model; if `TRUE`, the `a` parameter describes the
#'  `k` parameter in the model (the intercept).
#' @param use_minmax If `FALSE` (the default), the optimiser targets the sum
#'  of the goodness-of-fit values across datasets; if `TRUE`, it instead
#'  targets the maximum of the goodness-of-fit values across datasets.
#' @returns A numeric vector.
#' @examples
#' data <- data.frame(name = "test", promptness = rnorm(100, 3, 1))
#' fit <- fit_data(data = data)
#' data_other <- data.frame(name = "test_2", promptness = rnorm(100, 1, 1))
#' fit_shared_sigma <- fit_data(data = rbind(data, data_other), share_sigma = TRUE)
fit_data <- function(
  data,
  share_a = FALSE,
  share_sigma = FALSE,
  share_sigma_e = FALSE,
  with_early_component = FALSE,
  intercept_form = FALSE,
  use_minmax = FALSE
) {

  # initialise a container with the provided arguments
  fit_info <- list(
    share_a = share_a,
    share_sigma = share_sigma,
    share_sigma_e = share_sigma_e,
    with_early_component = with_early_component,
    intercept_form = intercept_form,
    use_minmax = use_minmax
  )

  # we only need to deal with the `name` and `promptness` columns in
  # the supplied data
  data <- data.frame(
    name = data$name,
    promptness = data$promptness,
    name_factor = factor(data$name)
  )

  fit_info$datasets <- levels(data$name_factor)
  fit_info$n_datasets <- length(fit_info$datasets)

  # only support fitting one or two datasets at a time
  stopifnot(fit_info$n_datasets %in% c(1, 2))

  # work out the number of model parameters for the provided number of
  # datasets and shared parameter arrangement
  fit_info <- set_param_counts(fit_info = fit_info)

  # calculate the ECDF and evaluate at each measurement
  data <- data |>
    dplyr::group_by(.data$name) |>
    dplyr::mutate(
      ecdf_p = stats::ecdf(.data$promptness)(.data$promptness)
    )

  # add new columns to the dataframe describing the parameter index for
  # each measurement
  data <- set_data_param_indices(data = data, fit_info = fit_info)

  # determine reasonable parameter values to start the optimisation
  fit_info$start_points <- calc_start_points(data = data, fit_info = fit_info)

  # run the optimiser
  fit_info$optim_result <- optim(
    fit_info$start_points,
    objective_function,
    data = data,
    fit_info = fit_info,
  )

  # convert the vector of parameter values into named parameters
  fit_info$fitted_params <- unpack_params(
    params = fit_info$optim_result$par,
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

  # add the `s` parameter as the inverse of sigma
  fit_info$fitted_params$s <- 1 / fit_info$fitted_params$sigma

  # compute the overall fit log-likelihood
  fit_info$loglike <- calc_loglike(data = data, fit_info = fit_info)

  return(list(f=fit_info,d=data))
  return(fit_info)

}


model_cdf <- function(q, later_mu, later_sd, early_sd = NULL) {

  if (is.null(early_sd)) {
    p <- pnorm(q = q, mean = later_mu, sd = later_sd)
  }
  else {
    p <- pnorm_with_early(
      q = q,
      later_mu = later_mu,
      later_sd = later_sd,
      early_sd = early_sd
    )
  }

  return(p)

}

model_pdf <- function(x, later_mu, later_sd, early_sd = NULL, log = FALSE) {

  if(is.null(early_sd)) {
    p <- dnorm(x = x, mean = later_mu, sd = later_sd, log = log)
  }
  else {
    p <- dnorm_with_early(
      x = x,
      later_mu = later_mu,
      later_sd = later_sd,
      early_sd = early_sd,
      log = log
    )
  }

  return(p)

}

calc_loglike <- function(data, fit_info) {

  loglike <- data |>
    dplyr::group_by(.data$name) |>
    dplyr::mutate(
      loglike = model_pdf(
        x = .data$promptness,
        later_mu = fit_info$fitted_params$mu[.data$i_mu],
        later_sd = fit_info$fitted_params$sigma[.data$i_sigma],
        early_sd = fit_info$fitted_params$sigma_e[.data$i_sigma_e],
        log = TRUE
      ),
    ) |>
    dplyr::pull(loglike)

  return(sum(loglike))
  
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

  ks <- data |>
    dplyr::group_by(.data$name) |>
    dplyr::mutate(
      # calculate the expected cumulative probability of each data point
      # under the model
      p = model_cdf(
        q = .data$promptness,
        later_mu = labelled_params$mu[.data$i_mu],
        later_sd = labelled_params$sigma[.data$i_sigma],
        early_sd = labelled_params$sigma_e[.data$i_sigma_e]
      )
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

  # constrain the SDs to be >= 0
  early_sd <- max(early_sd, 0)
  later_sd <- max(later_sd, 0)

  # cdf of the maximum of two independent gaussians is the product of
  # their individual values
  early_p <- pnorm(q, early_mu, early_sd)
  later_p <- pnorm(q, later_mu, later_sd)
  p <- early_p * later_p

  return(p)

}


# evaluates the probability density function when there are both early
# and late components and the draw is given by the maximum of the two
dnorm_with_early <- function(x, later_mu, later_sd, early_sd, log = FALSE) {

  early_mu <- 0

  p <- (
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

  if (log) {
    p <- base::log(p)
  }

  return(p)

}

erf <- function(x) {
  return(2 * pnorm(x * sqrt(2)) - 1)
}


set_param_counts <- function(fit_info) {

  fit_info$n_a <- ifelse(fit_info$share_a, 1, fit_info$n_datasets)
  fit_info$n_sigma <- ifelse(fit_info$share_sigma, 1, fit_info$n_datasets)
  fit_info$n_sigma_e <- ifelse(
    fit_info$with_early_component,
    ifelse(fit_info$share_sigma_e, 1, fit_info$n_datasets),
    0
  )

  fit_info$n_mu <- ifelse(
    fit_info$intercept_form,
    fit_info$n_sigma,
    fit_info$n_a
  )

  return(fit_info)

}

set_data_param_indices <- function(data, fit_info) {

  i_dataset = as.integer(data$name_factor)

  # remember that `ifelse` is strange for vectors!
  data$i_mu <- if(fit_info$n_mu == 1) 1 else i_dataset
  data$i_sigma <- if(fit_info$share_sigma) 1 else i_dataset
  data$i_sigma_e <- if(fit_info$share_sigma_e) 1 else i_dataset

  return(data)

}


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
