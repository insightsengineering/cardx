% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ard_mcnemartest.R
\name{ard_mcnemartest}
\alias{ard_mcnemartest}
\title{ARD McNemar's Test}
\usage{
ard_mcnemartest(data, by, variables, ...)
}
\arguments{
\item{data}{(\code{data.frame})\cr
a data frame. See below for details.}

\item{by}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
column name to compare by.}

\item{variables}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
column names to be compared. Independent tests will
be computed for each variable.}

\item{...}{arguments passed to \code{stats::mcnemar.test(...)}}
}
\value{
ARD data frame
}
\description{
Analysis results data for McNemar's statistical test.
}
\details{
For the \code{ard_mcnemartest()} function, the data is expected to be one row per subject.
The data is passed as \code{stats::mcnemar.test(x = data[[variable]], y = data[[by]], ...)}.
Please use \code{table(x = data[[variable]], y = data[[by]])} to check the contingency table.
}
\examples{
\dontshow{if (cards::is_pkg_installed("broom", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
cards::ADSL |>
  ard_mcnemartest(by = "SEX", variables = "EFFFL")
\dontshow{\}) # examplesIf}
}
