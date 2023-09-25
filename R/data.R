#' LATER unit reaction time data
#'
#' Simulated data from SPIC
#'
#' @format ## `rt`
#' A vector of 200 reaction times
#' \describe{
#'   \item{rt}{Vector of reaction times in ms}
#' }
#' @source LATER 1 mu 5 sig 0.5 R, SPIC program, developed by R.H.S. Carpenter
"rt"

#' Two LATER units reaction time data
#'
#' Simulated data from SPIC
#'
#' @format ## `rt2`
#' A dataframe of 200 reaction times for two participants: mu3 and mu4
#' \describe{
#'   \item{time}{Reaction times in ms}
#'   \item{name}{Participant name}
#'   \item{color}{Color to use for plotting each participant}
#' }
#' @source mu3 = LATER 1 mu 3 sig 0.5 R, count = 200; mu4 = LATER 2 mu 4 sig 0.5 R, count = 200 - SPIC program, developed by R.H.S. Carpenter
"rt2"

#' Digitised data corresponding to Figure 2 of Carpenter and Williams (1995)
#'
#' @format ## `carpenter_williams_1995`
#' A dataframe of 20014 reaction times for participant a and 22518 reaction times for participant b
#' \describe{
#'   \item{participant}{Participant "name", either "a" or "b"}
#'   \item{condition}{Prior percentage probability of the target being in the location of the eye movement (p05, p10, p25, p50, p75, p90, or p95)}
#'   \item{time}{Saccadic latency in ms}
#' }
#' @source Carpenter, R. H., & Williams, M. L. L. (1995). Neural computation of log likelihood in control of saccadic eye movements. Nature, 377(6544), 59-62.
"carpenter_williams_1995"
