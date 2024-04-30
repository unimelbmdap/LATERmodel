#' Compares the goodness-of-fit of a set of fit outcomes.
#'
#' @details
#' The 'evidence ratio' is calculated as per Motulsky & Christopolous (2004),
#' p. 146.
#' @param fits A list where each item has a name that identifies the fit and a
#'  value given by the output of `LATERmodel::fit_data`.
#' @returns A list of fit comparison results, ordered such that the fit with
#'  the lowest AIC value is in the first row.
#' * `aic` contains the fit AIC values.
#' * `preferred_rel_fit_delta_aic` is the AIC value for the fit relative to
#'  the AIC of the fit with the lowest AIC (preferred AIC - current AIC).
#' * `preferred_rel_fit_evidence_ratio` is the evidence ratio for the fit with
#'  the lowest AIC relative to the current fit.
#' * `preferred` is a boolean that indicates whether the fit has the lowest
#'  AIC value among the fits (is 'preferred').
#' @examples
#' \donttest{
#' data <- rbind(
#'   data.frame(name = "test", time = 1000/rnorm(100, 3, 1)),
#'   data.frame(name = "test_2", time = 1000/rnorm(100, 1, 1))
#' ) |> dplyr::filter(time > 0)
#' data <- prepare_data(data)
#' fit_a <- fit_data(data = data, share_a = TRUE)
#' fit_b <- fit_data(data = data, share_sigma = TRUE)
#' comparison <- compare_fits(fits = list(a = fit_a, b = fit_b))
#' }
#' @export
compare_fits <- function(fits) {
  aics <- unlist(lapply(fits, (function(fit) fit$aic)))

  min_aic <- min(aics)

  delta_aics <- lapply(aics, (function(aic) min_aic - aic))

  evidence_ratios <- lapply(
    aics,
    (
      function(aic) {
        calc_evidence_ratio(
          aic_model_1 = min_aic, aic_model_2 = aic
        )
      })
  )

  preferred <- lapply(
    aics,
    (function(aic) aic == min_aic)
  )

  data <- data.frame(
    aic = aics,
    preferred_rel_fit_delta_aic = unlist(delta_aics),
    preferred_rel_fit_evidence_ratio = unlist(evidence_ratios),
    preferred = unlist(preferred)
  )

  # sort rows in order of AIC
  data <- data[order(data$aic), ]

  return(data)
}

# calculate the 'evidence ratio' between a pair of AIC values.
# the returned value is expressed in relation to `aic_model_1` being the
# 'more correct' model compared to `aic_model_2`.
calc_evidence_ratio <- function(aic_model_1, aic_model_2) {
  delta_aic <- aic_model_2 - aic_model_1

  # see Motulsky & Christopolous (2004), p. 146
  return(1 / exp(-0.5 * delta_aic))
}
