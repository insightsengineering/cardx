% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ard_aod_wald_test.R
\name{ard_aod_wald_test}
\alias{ard_aod_wald_test}
\title{ARD Wald Test}
\usage{
ard_aod_wald_test(
  x,
  tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
  ...
)
}
\arguments{
\item{x}{regression model object}

\item{tidy_fun}{(\code{function})\cr
a tidier. Default is \code{\link[broom.helpers:tidy_with_broom_or_parameters]{broom.helpers::tidy_with_broom_or_parameters}}}

\item{...}{arguments passed to \code{aod::wald.test(...)}}
}
\value{
data frame
}
\description{
Function takes a regression model object and calculates Wald
statistical test using \code{\link[aod:wald.test]{aod::wald.test()}}.
}
\examples{
\dontshow{if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("aod", "broom.helpers")))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
lm(AGE ~ ARM, data = cards::ADSL) |>
  ard_aod_wald_test()
\dontshow{\}) # examplesIf}
}
