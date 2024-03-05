#' ARD McNemar's Test
#'
#' @description
#' Analysis results data for McNemar's statistical test.
#'
#' @param data (`data.frame`)\cr
#'   a data frame. See below for details.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared.
#' @param ... arguments passed to `stats::mcnemar.test(...)`
#'
#' @return ARD data frame
#' @export
#'
#' @details
#' For the `ard_mcnemartest()` function, the data is expected to be one row per subject.
#' The data is passed as `stats::mcnemar.test(x = data[[variable]], y = data[[by]], ...)`.
#' Please use `table(x = data[[variable]], y = data[[by]])` to check the contingency table.
#'
#' @examplesIf cards::is_pkg_installed("broom", reference_pkg = "cardx")
#' cards::ADSL |>
#'   ard_mcnemartest(by = "SEX", variable = "EFFFL")
ard_mcnemartest <- function(data, by, variable, ...) {
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
  .format_mcnemartest_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions(
        stats::mcnemar.test(x = data[[variable]], y = data[[by]], ...) |>
          broom::tidy()
      ),
    ...
  )
}

#' Convert McNemar's test to ARD
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams stats::mcnemar.test
#' @param by (`string`)\cr by column name
#' @param variable (`string`)\cr variable column name
#' @param ... passed to `stats::mcnemar.test(...)`
#'
#' @return ARD data frame
#'
#' @examples
#' cardx:::.format_mcnemartest_results(
#'   by = "ARM",
#'   variable = "AGE",
#'   lst_tidy =
#'     cards::eval_capture_conditions(
#'       stats::mcnemar.test(cards::ADSL[["SEX"]], cards::ADSL[["EFFFL"]]) |>
#'         broom::tidy()
#'     )
#' )
#'
#' @keywords internal
.format_mcnemartest_results <- function(by, variable, lst_tidy, ...) {
  # build ARD ------------------------------------------------------------------
  ret <-
    cards::tidy_as_ard(
      lst_tidy = lst_tidy,
      tidy_result_names = c("statistic", "p.value", "method"),
      fun_args_to_record = c("correct"),
      formals = formals(asNamespace("stats")[["mcnemar.test"]]),
      passed_args = dots_list(...),
      lst_ard_columns = list(group1 = by, variable = variable, context = "mcnemartest")
    )

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_mcnemar_stat_labels(),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::tidy_ard_column_order()
}

.df_mcnemar_stat_labels <- function() {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "statistic", "X-squared Statistic",
    "parameter", "Degrees of Freedom",
    "p.value", "p-value",
  )
}
