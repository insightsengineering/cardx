#' ARD Poisson Test
#'
#' @description
#' Analysis results data for exact tests of a simple null hypothesis about the rate parameter
#' in Poisson distribution, or the ratio between two rate parameters.
#'
#' @param data (`data.frame`)\cr
#'   a data frame. See below for details.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   optional column name to compare by.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to be compared. Independent tests will be computed for
#'   each variable.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of the subject or participant ID.
#' @param conf.level (scalar `numeric`)\cr
#'   confidence level for confidence interval. Default is `0.95`.
#' @param ... arguments passed to `poisson.test(...)`
#'
#' @return ARD data frame
#' @name ard_stats_poisson_test
#'
#' @details
#' For the `ard_stats_poisson_test()` function, the data is expected to be one row per subject.
#' The data is passed as `poisson.test(data[[variable]] ~ data[[by]], paired = FALSE, ...)`.
#'
#' For the `ard_stats_paired_poisson_test()` function, the data is expected to be one row
#' per subject per by level. Before the test is calculated, the data are
#' reshaped to a wide format to be one row per subject.
#' The data are then passed as
#' `poisson.test(x = data_wide[[<by level 1>]], y = data_wide[[<by level 2>]], paired = TRUE, ...)`.
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))
#' cards::ADSL |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   ard_stats_poisson_test(by = "ARM", variables = "AGE")
#'
#' # constructing a paired data set,
#' # where patients receive both treatments
#' cards::ADSL[c("ARM", "AGE")] |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
#'   dplyr::arrange(USUBJID, ARM) |>
#'   ard_stats_paired_poisson_test(by = ARM, variables = AGE, id = USUBJID)
NULL

#' @rdname ard_stats_poisson_test
#' @export
ard_stats_poisson_test <- function(data, numerator, denominator, na.rm = TRUE, by = NULL, conf.level = 0.95, ...) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed("broom", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(numerator)
  check_not_missing(denominator)
  check_data_frame(data)
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{ by }}, numerator = {{ numerator }}, denominator = {{ denominator }})
  check_logical(na.rm)
  check_scalar(by, allow_empty = TRUE)
  check_range(conf.level, range = c(0, 1))

  # check number of levels in `by`
  if (dplyr::n_distinct(data[[by]], na.rm = TRUE) != 2L) {
    cli::cli_abort(
      "The {.arg by} argument must have a maximum of two levels.",
      call = get_cli_abort_call()
    )
  }

  # calculate numerator and denominator values
  if (!is_empty(by)) {
    num <- data |>
      dplyr::group_by(.data[[by]]) |>
      dplyr::summarise(sum = sum(.data[[numerator]], na.rm = na.rm)) |>
      dplyr::pull(sum)
    denom <- data |>
      dplyr::group_by(.data[[by]]) |>
      dplyr::summarise(sum = sum(.data[[denominator]], na.rm = na.rm)) |>
      dplyr::pull(sum)
  } else {
    num <- sum(data[[numerator]], na.rm = na.rm)
    denom <- sum(data[[denominator]], na.rm = na.rm)
  }

  # build ARD ------------------------------------------------------------------
  .format_poissontest_results(
    by = by,
    lst_tidy =
      cards::eval_capture_conditions(
        stats::poisson.test(x = num, T = denom, conf.level = conf.level, ...) |> broom::tidy()
      ),
    ...
  )
}

#' Convert Poisson test to ARD
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams stats::poisson.test
#' @param by (`string`)\cr by column name
#' @param variable (`string`)\cr variable column name
#' @param ... passed to `stats::poisson.test(...)`
#'
#' @return ARD data frame
#' @keywords internal
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))
#' cardx:::.format_poissontest_results(
#'   by = "ARM",
#'   lst_tidy =
#'     cards::eval_capture_conditions(
#'       stats::poisson.test(sum(cards::ADTTE[["CNSR"]]), sum(cards::ADTTE[["AVAL"]])) |>
#'         broom::tidy()
#'     )
#' )
.format_poissontest_results <- function(by = NULL, lst_tidy, ...) {
  # build ARD ------------------------------------------------------------------
  ret <-
    cards::tidy_as_ard(
      lst_tidy = lst_tidy,
      tidy_result_names =
        c(
          "estimate", "statistic",
          "p.value", "parameter", "conf.low", "conf.high",
          "method", "alternative"
        ),
      fun_args_to_record = c("conf.level", "r"),
      formals = formals(asNamespace("stats")[["poisson.test"]]),
      passed_args = dots_list(...),
      lst_ard_columns = list(context = "stats_poisson_test")
    )

  # rename "r" statistic to "mu"
  ret$stat_name[ret$stat_name == "r"] <- "mu"

  if (!is_empty(by)) {
    ret <- ret |>
      dplyr::mutate(group1 = by)
  }

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_poissontest_stat_labels(by = by),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::tidy_ard_column_order()
}

.df_poissontest_stat_labels <- function(by = NULL) {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "estimate", ifelse(is_empty(by), "Estimated Rate", "Estimated Rate Ratio"),
    "statistic", ifelse(is_empty(by), "Number of Events", "Number of Events in First Sample"),
    "p.value", "p-value",
    "parameter", "Expected Count",
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "mu", "H0 Mean",
    "conf.level", "CI Confidence Level"
  )
}
