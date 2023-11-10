#' Simulate a dataset given model parameters.
#'
#' Generates samples (reaction times) from a set of provided LATER model parameters,
#' with the option to iteratively replace invalid samples (reaction times <= 0).
#'
#' @param n Number of samples (trials).
#' @param later_mu Mean of the later component.
#' @param later_sd Standard deviation of the later component.
#' @param early_sd Standard deviation of the early component,
#'  or `NULL` if there is no early component (the default).
#' @param seed Seed for the random number generator.
#' @param require_positive_times If `TRUE`, re-sample until all samples are > 0.
#' @returns Vector of reaction times (in seconds).
#' @examples
#' simulate_dataset(n = 100, later_mu = 5, later_sd = 1)
#' simulate_dataset(n = 100, later_mu = 5, later_sd = 1, early_sd = 5)
#' @export
simulate_dataset <- function(
    n,
    later_mu,
    later_sd,
    early_sd = NULL,
    seed = NULL,
    require_positive_times = TRUE) {
  if (is.null(seed)) {
    seed <- sample.int(n = .Machine$integer.max, size = 1)
  }

  has_early <- !is.null(early_sd)

  withr::with_seed(
    seed = seed,
    code = {
      later_draws <- draw_samples(
        n = n,
        mean = later_mu,
        sd = later_sd,
        require_positive_times = require_positive_times
      )
      if (has_early) {
        early_draws <- draw_samples(
          n = n,
          mean = 0,
          sd = early_sd,
          require_positive_times = require_positive_times
        )
        draws <- pmax(later_draws, early_draws)
      } else {
        draws <- later_draws
      }
    }
  )

  times <- 1 / draws

  return(times)
}

draw_samples <- function(n, mean, sd, require_positive_times = TRUE) {

  samples <- stats::rnorm(
      n = n, mean = mean, sd = sd
  )

  repeat({

    n_lte_zero <- sum(samples <= 0)

    if (!require_positive_times || n_lte_zero == 0) { 
      break
    }
    samples[samples <= 0] <- stats::rnorm(
      n = n_lte_zero, mean = mean, sd = sd
    )
  })

  return(samples)
}
