% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ard_stats_poisson_test.R
\name{ard_stats_poisson_test}
\alias{ard_stats_poisson_test}
\title{ARD Poisson Test}
\usage{
ard_stats_poisson_test(
  data,
  variables,
  na.rm = TRUE,
  by = NULL,
  conf.level = 0.95,
  ...
)
}
\arguments{
\item{data}{(\code{data.frame})\cr
a data frame. See below for details.}

\item{variables}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
names of the event and time variables (in that order) to be used in computations. Must be of length 2.}

\item{na.rm}{(scalar \code{logical})\cr
whether missing values should be removed before computations. Default is \code{TRUE}.}

\item{by}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
optional column name to compare by.}

\item{conf.level}{(scalar \code{numeric})\cr
confidence level for confidence interval. Default is \code{0.95}.}

\item{...}{arguments passed to \code{\link[=poisson.test]{poisson.test()}}.}
}
\value{
an ARD data frame of class 'card'
}
\description{
Analysis results data for exact tests of a simple null hypothesis about the rate parameter
in Poisson distribution, or the comparison of two rate parameters.
}
\details{
\itemize{
\item For the \code{ard_stats_poisson_test()} function, the data is expected to be one row per subject.
\item If \code{by} is not specified, an exact Poisson test of the rate parameter will be performed. Otherwise, a
Poisson comparison of two rate parameters will be performed on the levels of \code{by}. If \code{by} has more than 2
levels, an error will occur.
}
}
\examples{
\dontshow{if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Exact test of rate parameter against null hypothesis
cards::ADTTE |>
  ard_stats_poisson_test(variables = c(CNSR, AVAL))

# Comparison test of ratio of 2 rate parameters against null hypothesis
cards::ADTTE |>
  dplyr::filter(TRTA \%in\% c("Placebo", "Xanomeline High Dose")) |>
  ard_stats_poisson_test(by = TRTA, variables = c(CNSR, AVAL))
\dontshow{\}) # examplesIf}
}
