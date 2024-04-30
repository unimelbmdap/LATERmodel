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
#' @param fit_criterion String indicating the criterion used to optimise the
#'  fit by seeking its minimum.
#'   * `ks`: Kolmogorov-Smirnov statistic.
#'   * `likelihood`: Negative log-likelihood.
#' @param jitter_settings Settings for running the fitting multiple times with
#'   randomly-generated offsets ('jitter') applied to the starting estimates.
#'   * `n`: How many jitter iterations to run (default of 7); the total number
#'   of fits is `n + 1` (because the un-jittered start points are also fit).
#'   * `prop`: The maximum jitter offset, as a proportion of the start
#'   value (default of 0.5).
#'   * `seed`: Seed for the random jitter generator (default is unseeded).
#'   * `processes`: Maximum number of CPU processes that can be used (default
#'   is 2).
#' @returns A list of fitting arguments and outcomes.
#' * `fitted_params` is a named list of fitted parameter values.
#' * `named_fit_params` is a data frame with rows given by the dataset names
#'   and columns given by the parameter names.
#' * `loglike` is the overall log-likelihood of the fit.
#' * `aic` is the "Akaike's 'An Information Criterion'" value for the model.
#' * `optim_result` is the raw output from `stats::optim` for the best fit.
#' * `jitter_optim_results` contains the raw output from each call to
#'   `stats::optim` for the different start points.
#' @examples
#' \donttest{
#' data <- data.frame(name = "test", promptness = rnorm(100, 3, 1))
#' data_other <- data.frame(name = "test_2", promptness = rnorm(100, 1, 1))
#' fit_shared_sigma <- fit_data(
#'   data = rbind(data, data_other), share_sigma = TRUE
#' )
#' }
#' @export
fit_data <- function(
    data,
    share_a = FALSE,
    share_sigma = FALSE,
    share_sigma_e = FALSE,
    with_early_component = FALSE,
    intercept_form = FALSE,
    use_minmax = FALSE,
    fit_criterion = "likelihood",
    jitter_settings = list(n = 7, prop = 0.5, seed = NA, processes = 2)) {
  # only support fitting KS or neg likelihood criteria
  if (!(fit_criterion %in% c("ks", "likelihood"))) {
    rlang::abort("Fit criterion must be `ks` or `likelihood`")
  }

  # merge any provided jitter settings with their defaults
  jitter_settings <- merge_jitter_settings(jitter_settings = jitter_settings)

  # initialise a container with the provided arguments
  fit_info <- list(
    share_a = share_a,
    share_sigma = share_sigma,
    share_sigma_e = share_sigma_e,
    with_early_component = with_early_component,
    intercept_form = intercept_form,
    use_minmax = use_minmax,
    fit_criterion = fit_criterion,
    jitter_settings = jitter_settings
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

  fit_info$multiple_ds_no_share_warning <- (
    fit_info$n_datasets > 1 &&
      !any(share_a, share_sigma, share_sigma_e)
  )

  if (fit_info$multiple_ds_no_share_warning) {
    rlang::abort(
      "Multiple datasets were provided, but there are no shared parameters"
    )
  }

  # work out the number of model parameters for the provided number of
  # datasets and shared parameter arrangement
  fit_info <- set_param_counts(fit_info = fit_info)

  if (fit_info$fit_criterion == "ks") {
    data <- add_ecdf_to_data(data = data)
  }

  # add new columns to the dataframe describing the parameter index for
  # each measurement
  data <- set_data_param_indices(data = data, fit_info = fit_info)

  # determine reasonable parameter values to start the optimisation
  fit_info$start_points <- calc_start_points(data = data, fit_info = fit_info)

  # calculate a set of 'jitters' to apply to the start points
  fit_info$jitters <- gen_jitters(
    start_points = fit_info$start_points,
    jitter_amount_prop = fit_info$jitter_settings$prop,
    n_jitters = fit_info$jitter_settings$n,
    seed = fit_info$jitter_settings$seed
  )

  # start out with the start points intact (without any jitter)
  fit_info$jitters <- append(
    list(rep.int(0, length(fit_info$start_points))),
    fit_info$jitters
  )

  # initialise the 'cluster' to run the jitter fits in parallel
  # note that this is OS-dependent: it uses the socket-based type on
  # windows (because fork doesn't work on windows) and a fork-based
  # method on Linux (because psock doesn't work, reliably, on linux)
  cluster_type <- ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK")

  # don't use more workers than are necessary for the number of parallel fits
  n_workers <- min(jitter_settings$processes, jitter_settings$n + 1)

  cluster <- parallel::makeCluster(n_workers, type = cluster_type)

  # make the cluster processes aware of the necessary variables and functions
  # only needed for the PSOCK cluster type
  if (cluster_type == "PSOCK") {
    parallel::clusterExport(
      cl = cluster,
      varlist = list(
        "fit_info",
        "data",
        "model_cdf",
        "model_pdf",
        "calc_loglike",
        "unpack_params",
        "add_named_fit_params",
        "convert_a_to_mu_and_k",
        "objective_function",
        "calc_ks_stat",
        "pnorm_with_early",
        "dnorm_with_early",
        "erf",
        "set_param_counts",
        "set_data_param_indices",
        "merge_jitter_settings"
      ),
      envir = environment()
    )
  }

  fit_info$jitter_optim_results <- parallel::parLapply(
    cl = cluster,
    X = fit_info$jitters,
    fun = {
      function(jitter) {
        start_points <- fit_info$start_points + jitter

        # run the optimiser
        optim_result <- stats::optim(
          start_points,
          objective_function,
          control = list(maxit = 1000000),
          data = data,
          fit_info = fit_info
        )

        return(optim_result)
      }
    }
  )

  parallel::stopCluster(cl = cluster)

  # work out which of the jittered fits is the 'best' (has the lowest value)
  fit_info$i_best_jitter <- which.min(
    lapply(
      X = fit_info$jitter_optim_results,
      FUN = {
        function(x) {
          x$value
        }
      }
    )
  )

  fit_info$optim_result <- (
    fit_info$jitter_optim_results[[fit_info$i_best_jitter]]
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

  # calculate a data table for the fitted parameters, taking into account the
  # sharing of parameters
  fit_info$named_fit_params <- add_named_fit_params(fit_info = fit_info)

  # compute the overall fit log-likelihood
  fit_info$loglike <- calc_loglike(data = data, fit_info = fit_info)
  # and the AIC
  fit_info$aic <- calc_aic(
    loglike = fit_info$loglike,
    n_params = fit_info$n_params
  )

  return(fit_info)
}


#' Evalulate the cumulative distribution function under the model.
#'
#' @param q Vector of quantiles
#' @param later_mu Vector of the mean of the later component.
#' @param later_sd Vector of the standard deviation of the later component.
#' @param early_sd Vector of the standard deviation of the early component,
#'  or `NULL` if there is no early component (the default).
#' @returns Vector of cumulative distribution values
#' @examples
#' model_cdf(q = 1, later_mu = 1, later_sd = 1)
#' model_cdf(q = 1, later_mu = 1, later_sd = 1, early_sd = 3)
#' @export
model_cdf <- function(q, later_mu, later_sd, early_sd = NULL) {
  if (is.null(early_sd)) {
    p <- stats::pnorm(q = q, mean = later_mu, sd = later_sd)
  } else {
    p <- pnorm_with_early(
      q = q,
      later_mu = later_mu,
      later_sd = later_sd,
      early_sd = early_sd
    )
  }

  return(p)
}


#' Evalulate the probability density function under the model.
#'
#' @param x Vector of quantiles
#' @param later_mu Vector of the mean of the later component.
#' @param later_sd Vector of the standard deviation of the later component.
#' @param early_sd Vector of the standard deviation of the early component,
#'  or `NULL` if there is no early component (the default).
#' @param log If `TRUE`, probabilities are given as `log(p)`.
#' @returns Vector of probabilities
#' @examples
#' model_pdf(x = 1, later_mu = 1, later_sd = 1)
#' model_pdf(x = 1, later_mu = 1, later_sd = 1, early_sd = 3)
#' @export
model_pdf <- function(x, later_mu, later_sd, early_sd = NULL, log = FALSE) {
  if (is.null(early_sd)) {
    p <- stats::dnorm(x = x, mean = later_mu, sd = later_sd, log = log)
  } else {
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


#' Compute the empirical cumulative distribution function for promptness
#'
#' @param promptness A vector of promptness values (1 / times)
#' @param adjust_for_times If `TRUE` (the default), the returned `y` value is
#'  such that `1 - y = P(1/promptness <= 1/x). If `FALSE`, the returned
#'  `y` value is such that `y = P(promptness <= x)`.
#' @param eval_unique If `FALSE` (the default), the ECDF is evaluated at all
#' values in `promptness`. If `TRUE`, the ECDF is evaluated at the unique
#' values in `promptness`.
#' @returns A data frame with attributes:
#' * `x` is the values at which the ECDF was evaluated.
#' * `y` is the evaluated value of the ECDF.
#' @examples
#' p <- promptness_ecdf(promptness = rnorm(100, 3, 1))
#' @export
promptness_ecdf <- function(
    promptness,
    adjust_for_times = TRUE,
    eval_unique = FALSE) {
  x <- if (eval_unique) unique(promptness) else promptness

  if (adjust_for_times) {
    ecdf <- stats::ecdf(1 / promptness)
    y <- 1 - ecdf(1 / x)
  } else {
    ecdf <- stats::ecdf(promptness)
    y <- ecdf(x)
  }

  return(data.frame(x = x, y = y))
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


# calculate Akaike's 'An Information Criterion'
calc_aic <- function(loglike, n_params) {
  k <- 2
  aic <- -2 * loglike + k * n_params
  return(aic)
}


# parses a vector of parameters into a named list
unpack_params <- function(params, n_a, n_sigma, n_sigma_e) {
  # first `n_a` items are the a parameters
  a <- params[1:n_a]
  # next are the sigma parameters
  # note that the log of sigma is used in the optimiser
  log_sigma <- params[(n_a + 1):(n_a + n_sigma)]
  sigma <- exp(log_sigma)

  labelled_params <- list(a = a, sigma = sigma)

  if (n_sigma_e > 0) {
    # the sigma_e parameter is represented as the log of a multiplier of sigma
    log_sigma_e_mult <- params[(n_a + n_sigma + 1):length(params)]
    sigma_e_mult <- exp(log_sigma_e_mult)
    sigma_e <- sigma * sigma_e_mult
    labelled_params$sigma_e <- sigma_e
  }

  return(labelled_params)
}


add_named_fit_params <- function(fit_info) {
  df <- data.frame(
    lapply(
      X = fit_info$fitted_params,
      FUN = (function(x) rep(x, length.out = fit_info$n_datasets))
    ),
    row.names = fit_info$datasets
  )

  return(df)
}


convert_a_to_mu_and_k <- function(a, sigma, intercept_form) {
  if (intercept_form) {
    k <- a
    mu <- k * sigma
  } else {
    mu <- a
    k <- mu / sigma
  }

  return(list(mu = mu, k = k))
}


# returns the test statistic given a set of model parameter values
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

  if (fit_info$fit_criterion == "ks") {
    fit_val <- data |>
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
      dplyr::pull(.data$ks)
  } else if (fit_info$fit_criterion == "likelihood") {
    fit_val <- data |>
      dplyr::group_by(.data$name) |>
      dplyr::mutate(
        loglike = model_pdf(
          x = .data$promptness,
          later_mu = labelled_params$mu[.data$i_mu],
          later_sd = labelled_params$sigma[.data$i_sigma],
          early_sd = labelled_params$sigma_e[.data$i_sigma_e],
          log = TRUE
        ),
      ) |>
      dplyr::summarize(
        neg_loglike = -1 * sum(
          ifelse(.data$loglike == -Inf, -1e12, .data$loglike)
        )
      ) |>
      dplyr::pull(.data$neg_loglike)
  } else {
    stop("Unknown fit criterion")
  }

  if (fit_info$use_minmax) {
    # "minimise the worst of the fits"
    fit_val <- max(fit_val)
  } else {
    # "minimise the overall goodness-of-fit statistic"
    fit_val <- sum(fit_val)
  }

  return(fit_val)
}


# uses the sample mean and standard deviation of the promptness values
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
      dplyr::summarize(
        val = dplyr::if_else(
          condition = moments::skewness(.data$promptness) < 0.25,
          true = stats::sd(.data$promptness),
          false = stats::sd(.data$promptness) / 2
        )
      ) |>
      dplyr::pull(.data$val)
  )

  log_sigma_values <- log(sigma_values)

  if (fit_info$intercept_form) {
    a_values <- mu_values / sigma_values
    if (fit_info$n_a == 1) {
      a_values <- mean(a_values)
    }
  } else {
    a_values <- mu_values
  }

  start_points <- c(a_values, log_sigma_values)

  if (fit_info$with_early_component) {
    sigma_e_mult_values <- (
      data |>
        dplyr::group_by(.data$i_sigma_e) |>
        dplyr::summarize(
          val = dplyr::if_else(
            condition = moments::skewness(.data$promptness) < 0.25,
            true = 0.5,
            false = 5
          )
        ) |>
        dplyr::pull(.data$val)
    )

    log_sigma_e_mult_values <- log(sigma_e_mult_values)

    start_points <- c(start_points, log_sigma_e_mult_values)
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
  early_p <- stats::pnorm(q, early_mu, early_sd)
  later_p <- stats::pnorm(q, later_mu, later_sd)
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
        exp(-(((x - later_mu)**2) / (2 * later_sd**2)))
        * (1 + erf((x - early_mu) / (sqrt(2) * early_sd)))
      ) / later_sd
        + (
          exp(-(((x - early_mu)**2) / (2 * early_sd**2)))
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
  return(2 * stats::pnorm(x * sqrt(2)) - 1)
}


# works out how many parameters there are, given the sharing amongst
# the parameters
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
    fit_info$n_datasets,
    fit_info$n_a
  )

  fit_info$n_params <- fit_info$n_a + fit_info$n_sigma + fit_info$n_sigma_e

  return(fit_info)
}


# creates `i_mu`, `i_sigma`, and `i_sigma_e` columns in the data
# that describe the indices of the relevant parameters
set_data_param_indices <- function(data, fit_info) {
  i_dataset <- as.integer(data$name_factor)

  # remember that `ifelse` is strange for vectors!
  data$i_mu <- if (fit_info$n_mu == 1) 1 else i_dataset
  data$i_sigma <- if (fit_info$share_sigma) 1 else i_dataset
  data$i_sigma_e <- if (fit_info$share_sigma_e) 1 else i_dataset

  return(data)
}


# calculate the ECDF and evaluate at each measurement
add_ecdf_to_data <- function(data) {
  data <- data |>
    dplyr::group_by(.data$name) |>
    dplyr::mutate(
      ecdf_p = promptness_ecdf(.data$promptness)$y,
    )

  return(data)
}


# generate a set of 'jitters' (offsets to apply to the start points)
gen_jitters <- function(
    start_points,
    jitter_amount_prop,
    n_jitters,
    seed = NA) {
  if (n_jitters == 0) {
    return(list())
  }

  seed <- ifelse(
    is.na(seed),
    sample.int(n = .Machine$integer.max, size = 1),
    seed
  )

  # one seed for each jitter (where a 'jitter' is a complete set of offsets)
  jitter_seeds <- withr::with_seed(
    seed = seed,
    code = {
      sample.int(n = .Machine$integer.max, size = n_jitters)
    }
  )

  # the maximum offset is a proportion of the start point
  max_offset <- abs(start_points * jitter_amount_prop)

  # each offset is sampled from a uniform distribution
  # in [-max_offset, +max_offset]
  jitter_amounts <- lapply(
    X = jitter_seeds,
    FUN = {
      function(jitter_seed) {
        withr::with_seed(
          seed = jitter_seed,
          code = {
            stats::runif(
              n = length(start_points),
              min = -max_offset,
              max = +max_offset
            )
          }
        )
      }
    }
  )

  return(jitter_amounts)
}


merge_jitter_settings <- function(jitter_settings) {
  if (!("n" %in% names(jitter_settings))) {
    jitter_settings$n <- 7
  }

  if (!("prop" %in% names(jitter_settings))) {
    jitter_settings$prop <- 0.5
  }

  if (!("seed" %in% names(jitter_settings))) {
    jitter_settings$seed <- NA
  }

  if (!("processes" %in% names(jitter_settings))) {
    jitter_settings$processes <- 2
  }

  return(jitter_settings)
}
