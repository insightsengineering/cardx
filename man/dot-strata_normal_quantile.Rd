% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proportion_ci.R
\name{.strata_normal_quantile}
\alias{.strata_normal_quantile}
\title{Helper Function for the Estimation of Stratified Quantiles}
\usage{
.strata_normal_quantile(vars, weights, conf.level)
}
\arguments{
\item{weights}{(\code{numeric})\cr weights for each level of the strata. If \code{NULL}, they are
estimated using the iterative algorithm that
minimizes the weighted squared length of the confidence interval.}

\item{conf.level}{(scalar \code{numeric})\cr
a scalar in \verb{(0,1)} indicating the confidence level.
Default is \code{0.95}}
}
\value{
Stratified quantile.
}
\description{
This function wraps the estimation of stratified percentiles when we assume
the approximation for large numbers. This is necessary only in the case
proportions for each strata are unequal.
}
\examples{
strata_data <- table(data.frame(
  "f1" = sample(c(TRUE, FALSE), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  stringsAsFactors = TRUE
))
ns <- colSums(strata_data)
ests <- strata_data["TRUE", ] / ns
vars <- ests * (1 - ests) / ns
weights <- rep(1 / length(ns), length(ns))

cardx:::.strata_normal_quantile(vars, weights, 0.95)
}
\seealso{
\code{\link[=proportion_ci_strat_wilson]{proportion_ci_strat_wilson()}}
}
\keyword{internal}
