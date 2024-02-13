#' Simulate a dataset given model parameters.
#'
#' Generates samples from a set of provided LATER model parameters,
#' with the option to iteratively replace invalid samples (reaction
#' times <= 0).
#'
#' @param n Number of samples (trials)
#' @param later_mu Mean of the later component.
#' @param later_sd Standard deviation of the later component.
#' @param early_sd Standard deviation of the early component,
#'  or `NULL` if there is no early component (the default).
#' @param seed Seed for the random number generator
#' @param allow_negative_times If `FALSE` (the default), any random samples
#'  that have negative response times are iteratively replaced such that all
#'  returned samples are positive. If `TRUE`, no such replacement is performed.
#' @returns Vector of response times (in seconds)
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
    allow_negative_times = FALSE) {
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
        allow_negative_times = allow_negative_times
      )
      if (has_early) {
        early_draws <- draw_samples(
          n = n,
          mean = 0,
          sd = early_sd,
          allow_negative_times = allow_negative_times
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

# re-draws if sample is <= 0 and `allow_negative_times` is `FALSE`
draw_samples <- function(n, mean, sd, allow_negative_times = FALSE) {
  samples <- stats::rnorm(
    n = n, mean = mean, sd = sd
  )

  repeat({
    n_lte_zero <- sum(samples <= 0)

    if (n_lte_zero == 0 || allow_negative_times) {
      break
    }
    samples[samples <= 0] <- stats::rnorm(
      n = n_lte_zero, mean = mean, sd = sd
    )
  })

  return(samples)
}
