#' ARD Cochran-Mantel-Haenszel Chi-Squared Test
#'
#' @description
#' Analysis results data for Cochran-Mantel-Haenszel Chi-Squared Test for count data.
#' Calculated with `mantelhaen.test(x = data[[variable]], y = data[[by]], z = data[[strata]], ...)`
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to be compared. Independent tests will be computed for
#'   each variable.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to stratify by. This parameter is optional.
#' @param ... additional arguments passed to `mantelhaen.test(...)`
#'
#' @return ARD data frame
#' @name ard_stats_mantelhaen_test
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom"))
#' cards::ADSL |>
#'   ard_stats_mantelhaen_test(by = "ARM", variables = "AGEGR1", strata = "SEX")
#'
#' Rabbits |>
#'   ard_stats_mantelhaen_test(exact = TRUE, alternative = "greater")
NULL

#' @rdname ard_stats_mantelhaen_test
#' @export
ard_stats_mantelhaen_test <- function(data, ...) {
  set_cli_abort_call()

  check_not_missing(data)
  UseMethod("ard_stats_mantelhaen_test")
}

#' @rdname ard_stats_mantelhaen_test
#' @export
ard_stats_mantelhaen_test.data.frame <- function(data, by, variables, strata, ...) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed("broom")

  # check/process inputs -------------------------------------------------------
  check_not_missing(variables)
  check_not_missing(by)
  check_not_missing(strata)
  cards::process_selectors(data, by = {{ by }}, variables = {{ variables }}, strata = {{ strata }})
  check_scalar(by)
  check_scalar(strata)

  # return empty ARD if no variables selected ----------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble() |> cards::as_card())
  }

  # build ARD ------------------------------------------------------------------
  ret <- lapply(
    variables,
    function(variable) {
      cards::tidy_as_ard(
        lst_tidy =
          cards::eval_capture_conditions(
            stats::mantelhaen.test(
              x = data[[variable]],
              y = data[[by]],
              z = data[[strata]],
              ...
            ) |>
              broom::tidy()
          ),
        tidy_result_names = c("statistic", "p.value", "parameter", "method"),
        fun_args_to_record =
          c("alternative", "correct", "exact", "conf.level"),
        formals = formals(stats::mantelhaen.test),
        passed_args = dots_list(...),
        lst_ard_columns = list(group1 = by, group2 = strata, variable = variable, context = "stats_mantelhaen_test")
      )
    }
  ) |>
    dplyr::bind_rows()

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_mantelhaentest_stat_labels(exact = ret$stat$exact),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::as_card() |>
    cards::tidy_ard_column_order()
}

#' @rdname ard_stats_mantelhaen_test
#' @export
ard_stats_mantelhaen_test.array <- function(data, ...) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed("broom")

  # check/process inputs -------------------------------------------------------
  by <- names(dimnames(data))[1]
  variables <- names(dimnames(data))[2]
  strata <- names(dimnames(data))[3]

  # return empty ARD if no variables selected ----------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble() |> cards::as_card())
  }

  # build ARD ------------------------------------------------------------------
  ret <- lapply(
    variables,
    function(variable) {
      cards::tidy_as_ard(
        lst_tidy =
          cards::eval_capture_conditions(
            stats::mantelhaen.test(
              x = data,
              ...
            ) |>
              broom::tidy()
          ),
        tidy_result_names = c("statistic", "p.value", "parameter", "method"),
        fun_args_to_record =
          c("alternative", "correct", "exact", "conf.level"),
        formals = formals(stats::mantelhaen.test),
        passed_args = dots_list(...),
        lst_ard_columns = list(group1 = by, group2 = strata, variable = variable, context = "stats_mantelhaen_test")
      )
    }
  ) |>
    dplyr::bind_rows()

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_mantelhaentest_stat_labels(exact = ret$stat$exact),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::as_card() |>
    cards::tidy_ard_column_order()
}

.df_mantelhaentest_stat_labels <- function(exact = FALSE) {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "estimate", ifelse(exact, "Mantel-Haenszel Odds Ratio Estimate", "Conditional Maximum Likelihood Odds Ratio Estimate"),
    "statistic", ifelse(exact, "Mantel-Haenszel X-squared Statistic", "Generalized Cochran-Mantel-Haenszel Statistic"),
    "p.value", "p-value",
    "parameter", "Degrees of Freedom",
    "correct", "Continuity Correction",
    "exact", "Exact Conditional Test",
    "conf.level", "CI Confidence Level",
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound"
  )
}
