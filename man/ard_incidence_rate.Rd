% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ard_incidence_rate.R
\name{ard_incidence_rate}
\alias{ard_incidence_rate}
\title{ARD Incidence Rate}
\usage{
ard_incidence_rate(
  data,
  time,
  count = NULL,
  id = NULL,
  by = NULL,
  strata = NULL,
  n_person_time = 100,
  unit_label = "time",
  conf.level = 0.95,
  conf.type = c("normal", "normal-log", "exact", "byar")
)
}
\arguments{
\item{data}{(\code{data.frame})\cr
a data frame.}

\item{time}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
column name of time at risk variable.}

\item{count}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
column name of variable indicating count of events that occurred. If \code{NULL}, each row in \code{data} is assumed to
correspond to a single event occurrence.}

\item{id}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
column name used to identify unique subjects in \code{data}. If \code{NULL}, each row in \code{data} is assumed to correspond to
a unique subject.}

\item{by, strata}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
columns to tabulate by/stratify by for summary statistic
calculation. Arguments are similar, but with an important distinction:

\code{by}: results are calculated for \strong{all combinations} of the columns specified,
including unobserved combinations and unobserved factor levels.

\code{strata}: results are calculated for \strong{all \emph{observed} combinations} of the
columns specified.

Arguments may be used in conjunction with one another.}

\item{n_person_time}{(\code{numeric})\cr
amount of person-time to estimate incidence rate for. Defaults to 100.}

\item{unit_label}{(\code{string})\cr
label for the unit of values in \code{time} and estimated person-time output (e.g. \code{"years"} for person-years,
\code{"days"} for person-days, etc.). If the desired person-time estimate unit does not match the current \code{time} unit,
values of \code{time} should be converted to the correct unit during pre-processing. Defaults to \code{"time"} (person-time).}

\item{conf.level}{(\code{numeric})\cr
confidence level for the estimated incidence rate.}

\item{conf.type}{(\code{string})\cr
confidence interval type for the estimated incidence rate.

One of: \code{normal} (default), \code{normal-log}, \code{exact}, or \code{byar}.}
}
\value{
an ARD data frame of class 'card'
}
\description{
Function takes a time at risk variable (\code{time}) and event count variable (\code{count}) and calculates the incidence
rate in person-years.

Incidence rate is calculated as: Total number of events that occurred / Total person-time at risk
}
\details{
The formulas used to calculate the confidence interval for each CI type are as
follows, where \eqn{x_i} and \eqn{t_i} represent the number of events and follow-up
time for subject \eqn{i}, respectively.
\itemize{
\item \code{byar}: Byar's approximation of a Poisson CI. A continuity correction of 0.5 is included in the calculation.

\deqn{CI = (\sum{x_i} + 0.5) (1 - 1 / (9 \times (\sum{x_i} + 0.5)) \pm Z_{1 - \alpha / 2} / (3 \sqrt{\sum{x_i} + 0.5}))^3 / \sum{t_i}}
\item \code{normal}: Normal CI.

\deqn{CI = \sum{x_i} / \sum{t_i} \pm Z_{1 - \alpha / 2} \sqrt{\sum{x_i}} / \sum{t_i}}
\item \code{normal-log}: Normal-Log CI.

\deqn{CI = \exp(\log(\sum{x_i} / \sum{t_i}) \pm Z_{1 - \alpha / 2} / \sqrt{\sum{x_i}})}
\item \code{exact}: Exact CI for a Poisson mean.

\deqn{CI_{lower} = \chi^2_{\alpha / 2, 2\sum{x_i} + 2} / {2 \sum{t_i}}}
\deqn{CI_{upper} = \chi^2_{1 - \alpha / 2, 2\sum{x_i} + 2} / {2 \sum{t_i}}}
}
}
\examples{
set.seed(1)
data <- data.frame(
  USUBJID = 1:100,
  TRTA = sample(LETTERS[1:3], 100, replace = TRUE),
  AETTE1 = abs(rnorm(100, mean = 0.5)),
  AETOT1 = sample(0:20, 100, replace = TRUE)
)

data |>
  ard_incidence_rate(time = AETTE1, count = AETOT1, id = USUBJID, by = TRTA, unit_label = "years")
}
