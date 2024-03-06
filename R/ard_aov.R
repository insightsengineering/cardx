#' ARD ANOVA
#'
#' @description
#' Analysis results data for Analysis of Variance.
#' Calculated with `aov(data[[variable]] ~ as.factor(data[[by]]), data = data, ...)`
#'
#' @param x regression model object
#'
#' @return ARD data frame
#' @export
#'
#' @examples
#' lm(AGE ~ ARM, data = cards::ADSL) |>
#'   ard_aov()
ard_aov <- function(x) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(x)

  # build ARD ------------------------------------------------------------------
  cards::tidy_as_ard(
    lst_tidy =
      cards::eval_capture_conditions(
        stats::aov(x) |>
          broom::tidy() |>
          dplyr::slice_head()
      ),
    tidy_result_names = c("term", "df", "sumsq", "meansq", "statistic", "p.value"),
    formals = formals(stats::aov),
    lst_ard_columns = list(context = "aov")
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "term" ~ "Term",
          .data$stat_name %in% "sumsq" ~ "Sum of Squares",
          .data$stat_name %in% "meansq" ~ "Mean of Sum of Squares",
          .data$stat_name %in% "statistic" ~ "F Statistic",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "df" ~ "Degrees of Freedom",
          TRUE ~ .data$stat_name,
        )
    )
}
