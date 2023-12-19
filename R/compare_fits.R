#' Compares the goodness-of-fit of a set of fit outcomes.
#'
#' @param fits A list where each item has a name that identifies the fit and a
#'  value given by the output of `LATERmodel::fit_data`.
#' @returns 
#' @export
compare_fits <- function(fits) {

  aics <- unlist(lapply(fits, (function (fit) fit$aic)));

  min_aic <- min(aics);

  delta_aics <- lapply(aics, (function (aic) min_aic - aic));

  evidence_ratios <- lapply(
    aics,
    (function (aic) calc_evidence_ratio(aic_model_1 = min_aic, aic_model_2 = aic))
  );

  preferred <- lapply(
    aics,
    (function (aic) aic == min_aic));

  data <- data.frame(
    aic = aics,
    preferred_rel_fit_delta_aic = unlist(delta_aics),
    preferred_wrt_fit_evidence_ratio = unlist(evidence_ratios),
    preferred = unlist(preferred));

  data <- data[order(data$aic),];

  return(data);

}

# calculate the 'evidence ratio' between a pair of AIC values.
# the returned value is expressed in relation to `aic_model_1` being the
# 'more correct' model compared to `aic_model_2`.
calc_evidence_ratio <- function(aic_model_1, aic_model_2) {

  delta_aic <- aic_model_2 - aic_model_1;

  return(1 / exp(-0.5 * delta_aic));

}
