#' ARD ANOVA
#'
#' @description
#' Analysis results data for Analysis of Variance.
#' Calculated with `stats::aov()`
#'
#' @param x linear model formula
#' @param data (`data.frame`)\cr
#' a data frame containing the variables in `x`
#' @param ... arguments passed to `stats::aov(...)`
#'
#' @return ARD data frame
#' @export
#'
#' @examples
#' ard_aov(AGE ~ ARM, data = cards::ADSL)
ard_aov <- function(x, data, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed(c("broom.helpers"), reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(x)
  check_not_missing(data)
  check_data_frame(data)

  # build ARD ------------------------------------------------------------------
  aov <-
    cards::eval_capture_conditions(
      stats::aov(x, data, ...)
    )
  aov[["result"]] |>
    broom.helpers::tidy_parameters() |> # using broom.helpers, because it handle non-syntactic names
    dplyr::filter(!(dplyr::row_number() == dplyr::n() & .data$term %in% "Residuals")) |> # removing Residual rows
    dplyr::rename(variable = "term") |>
    tidyr::pivot_longer(
      cols = -"variable",
      names_to = "stat_name",
      values_to = "stat"
    ) |>
    dplyr::mutate(
      stat = as.list(.data$stat),
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "statistic" ~ "Statistic",
          .data$stat_name %in% "df" ~ "Degrees of Freedom",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "sumsq" ~ "Sum of Squares",
          .data$stat_name %in% "meansq" ~ "Mean of Sum of Squares",
          TRUE ~ .data$stat_name
        ),
      context = "aov",
      warning = aov["warning"],
      error = aov["error"]
    ) |>
    cards::tidy_ard_column_order() %>%
    {
      structure(., class = c("card", class(.)))
    }
}
