% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ard_stats_fisher_test.R
\name{ard_stats_fisher_test}
\alias{ard_stats_fisher_test}
\title{ARD Fisher's Exact Test}
\usage{
ard_stats_fisher_test(data, by, variables, conf.level = 0.95, ...)
}
\arguments{
\item{data}{(\code{data.frame})\cr
a data frame.}

\item{by}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
column name to compare by}

\item{variables}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
column names to be compared. Independent tests will be computed for
each variable.}

\item{conf.level}{(scalar \code{numeric})\cr
confidence level for confidence interval. Default is \code{0.95}.}

\item{...}{additional arguments passed to \code{fisher.test(...)}}
}
\value{
ARD data frame
}
\description{
Analysis results data for Fisher's Exact Test.
Calculated with \code{fisher.test(x = data[[variable]], y = data[[by]], ...)}
}
\examples{
\dontshow{if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
cards::ADSL[1:30, ] |>
  ard_stats_fisher_test(by = "ARM", variables = "AGEGR1")
\dontshow{\}) # examplesIf}
}
