#' ARD Cochran-Mantel-Haenszel Chi-Squared Test
#'
#' @description
#' Analysis results data for Cochran-Mantel-Haenszel Chi-Squared Test for count data.
#' Calculated with `mantelhaen.test(x = data[[variable]], y = data[[by]], z = data[[strata]], ...)` for data frames, and
#' `mantelhaen.test(x = data, ...)` for arrays.
#'
#' @param data (`data.frame` or `array`)\cr
#'   a data frame or 3-dimensional named array.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by. If `data` is an array `by` is ignored and the first dimension of `data` is used as `by`.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to be compared. Independent tests will be computed for each variable. If `data` is an array
#'   `variables` is ignored and the second dimension of `data` is used as `variables`.
#' @param strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to stratify by. If `data` is an array `strata` is ignored and the third dimension of `data` is used as
#'   `strata`.
#' @param ... additional arguments passed to `mantelhaen.test(...)`
#'
#' @return ARD data frame
#' @name ard_stats_mantelhaen_test
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom"))
#' # Data Frame Example ----------------------
#' cards::ADSL |>
#'   ard_stats_mantelhaen_test(by = "ARM", variables = "AGEGR1", strata = "SEX")
#'
#' # Array Example ---------------------------
#' penicillin <- array(
#'   c(0, 0, 6, 5, 3, 0, 3, 6, 6, 2, 0, 4, 5, 6, 1, 0, 2, 5, 0, 0),
#'   dim = c(2, 2, 5),
#'   dimnames = list(
#'     Delay = c("None", "1.5h"), Response = c("Cured", "Died"), Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4")
#'   )
#' )
#'
#' penicillin |>
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

  formals_cmh <- formals(asNamespace("stats")[["mantelhaen.test"]])
  if (!"alternative" %in% names(dots_list(...))) {
    formals_cmh$alternative <- "two.sided"
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
        formals = formals_cmh,
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
  if (length(dim(data)) != 3) {
    cli::cli_abort(
      "When {.arg data} is an {.cls array} it must be 3-dimensional, but {.arg data} is currently {length(dim(data))}-dimensional.",
      call = get_cli_abort_call()
    )
  }
  if (is.null(names(dimnames(data)))) {
    cli::cli_abort(
      "The array given as {.arg data} is unnamed but must have 3 named dimensions. The names will be assigned as the {.arg by}, {.arg variables}, and {.arg strata} column names, respectively.",
      call = get_cli_abort_call()
    )
  }
  by <- names(dimnames(data))[1]
  variable <- names(dimnames(data))[2]
  strata <- names(dimnames(data))[3]

  formals_cmh <- formals(asNamespace("stats")[["mantelhaen.test"]])
  if (!"alternative" %in% names(dots_list(...))) {
    formals_cmh$alternative <- "two.sided"
  }

  # build ARD ------------------------------------------------------------------
  ret <- cards::tidy_as_ard(
    lst_tidy =
      cards::eval_capture_conditions(
        stats::mantelhaen.test(
          x = data,
          ...
        ) |>
          broom::tidy() |>
          dplyr::select(-"alternative")
      ),
    tidy_result_names = c("statistic", "p.value", "parameter", "method"),
    fun_args_to_record =
      c("alternative", "correct", "exact", "conf.level"),
    formals = formals_cmh,
    passed_args = dots_list(...),
    lst_ard_columns = list(group1 = by, group2 = strata, variable = variable, context = "stats_mantelhaen_test")
  )

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
