% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_data.R
\name{ks_compare}
\alias{ks_compare}
\title{Apply two-sample KS test to all pairs of datasets contained within a
dataframe}
\usage{
ks_compare(df, correct_multiple_comparisons = TRUE)
}
\arguments{
\item{df}{A dataframe of datasets with columns: \code{name} and \code{time}, one
unique \code{name} per dataset}

\item{correct_multiple_comparisons}{If \code{TRUE}, an adjustment will be made
to the p-values based on Holm, 1979, A simple sequentially rejective
multiple test procedure}
}
\value{
A dataframe with columns \code{name1}, \code{name2}, \code{D}, and \code{p_value}
}
\description{
Apply two-sample KS test to all pairs of datasets contained within a
dataframe
}
\examples{
data <- prepare_data(dplyr::filter(
  carpenter_williams_1995,
  participant == "b"
))
ks_compare(data)
}
