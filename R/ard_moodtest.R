#' ARD Mood Test
#'
#' @description
#' Analysis results data for Mood two sample test of scale. Note this not to be confused with
#' the Brown-Mood test of medians.
#'
#' @param data (`data.frame`)\cr
#'   a data frame. See below for details.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared.
#' @param ... arguments passed to `mood.test(...)`
#'
#' @return ARD data frame
#' @name ard_moodtest
#'
#' @details
#' For the `ard_moodtest()` function, the data is expected to be one row per subject.
#' The data is passed as `mood.test(data[[variable]] ~ data[[by]], ...)`.
#' @rdname ard_moodtest
#' @export
#'
#' @examples
#' cards::ADSL |>
#'   ard_moodtest(by = "SEX", variable = "AGE")
ard_moodtest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(by)
  check_data_frame(data)
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_scalar(by)
  check_scalar(variable)

  # build ARD ------------------------------------------------------------------
  .format_moodtest_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions(
        stats::mood.test(data[[variable]] ~ data[[by]], ...) |>
          broom::tidy()
      ),
    ...
  )
}
#' Convert mood test results to ARD
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams stats::mood.test
#' @param by (`string`)\cr by column name
#' @param variable (`string`)\cr variable column name
#' @param ... passed to `mood.test(...)`
#'
#' @return ARD data frame
#' @keywords internal
#' @examples
#' cardx:::.format_moodtest_results(
#'   by = "SEX",
#'   variable = "AGE",
#'   lst_tidy =
#'     cards::eval_capture_conditions(
#'       stats::mood.test(ADSL[["AGE"]] ~ ADSL[["SEX"]]) |>
#'         broom::tidy()
#'     )
#' )
.format_moodtest_results <- function(by, variable, lst_tidy, ...) {
  # build ARD ------------------------------------------------------------------
  ret <-
    cards::tidy_as_ard(
      lst_tidy = lst_tidy,
      tidy_result_names = c("statistic", "p.value", "method", "alternative"),
      formals = formals(asNamespace("stats")[["mood.test.default"]]),
      passed_args = c(dots_list(...)),
      lst_ard_columns = list(group1 = by, variable = variable, context = "moodtest")
    )

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_moodtest_stat_labels(),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::tidy_ard_column_order()
}

.df_moodtest_stat_labels <- function() {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "statistic", "Z-Statistic",
    "p.value", "p-value",
    "alternative", "Alternative Hypothesis"
  )
}
