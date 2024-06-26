% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ard_stats_t_test.R
\name{.paired_data_pivot_wider}
\alias{.paired_data_pivot_wider}
\title{Convert long paired data to wide}
\usage{
.paired_data_pivot_wider(data, by, variable, id)
}
\arguments{
\item{data}{(\code{data.frame})\cr a data frame that is one line per subject per group}

\item{by}{(\code{string})\cr by column name}

\item{variable}{(\code{string})\cr variable column name}

\item{id}{(\code{string})\cr subject id column name}
}
\value{
a wide data frame
}
\description{
Convert long paired data to wide
}
\examples{
cards::ADSL[c("ARM", "AGE")] |>
  dplyr::filter(ARM \%in\% c("Placebo", "Xanomeline High Dose")) |>
  dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
  dplyr::arrange(USUBJID, ARM) |>
  cardx:::.paired_data_pivot_wider(by = "ARM", variable = "AGE", id = "USUBJID")
}
\keyword{internal}
