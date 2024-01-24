#' ARD t-test
#'
#' @description
#' Analysis results data for paired and non-paired t-tests.
#'
#' @param data (`data.frame`)\cr
#'   a data frame. See below for details.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of the subject or participant ID
#' @param ... arguments passed to `t.test(...)`
#'
#' @return ARD data frame
#' @name ard_ttest
#'
#' @details
#' For the `ard_ttest()` function, the data is expected to be one row per subject.
#' The data is passed as `t.test(data[[variable]] ~ data[[by]], paired = FALSE, ...)`.
#'
#' For the `ard_paired_ttest()` function, the data is expected to be one row
#' per subject per by level. Before the t-test is calculated, the data are
#' reshaped to a wide format to be one row per subject.
#' The data are then passed as
#' `t.test(x = data_wide[[<by level 1>]], y = data_wide[[<by level 2>]], paired = TRUE, ...)`.
#'
#' @examples
#' cards::ADSL |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   ard_ttest(by = "ARM", variable = "AGE")
#'
#' # constructing a paired data set,
#' # where patients receive both treatments
#' cards::ADSL[c("ARM", "AGE")] |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
#'   dplyr::arrange(USUBJID, ARM) |>
#'   ard_paired_ttest(by = ARM, variable = AGE, id = USUBJID)
NULL

#' @rdname ard_ttest
#' @export
ard_ttest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(by)
  check_class_data_frame(x = data)
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_scalar(by)
  check_scalar(variable)

  # build ARD ------------------------------------------------------------------
  .format_ttest_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions(
        stats::t.test(data[[variable]] ~ data[[by]], paired = FALSE, ...) |>
          broom::tidy()
      ),
    paired = FALSE,
    ...
  )
}

#' @rdname ard_ttest
#' @export
ard_paired_ttest <- function(data, by, variable, id, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(by)
  check_not_missing(id)
  check_class_data_frame(x = data)
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{ by }}, variable = {{ variable }}, id = {{ id }})
  check_scalar(by)
  check_scalar(variable)
  check_scalar(id)

  # build ARD ------------------------------------------------------------------
  .format_ttest_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions({
        data_wide <- .paired_data_pivot_wider(data, by = by, variable = variable, id = id)
        stats::t.test(x = data_wide[["by1"]], y = data_wide[["by2"]], paired = TRUE, ...) |>
          broom::tidy()
      }),
    paired = TRUE,
    ...
  )
}

#' Convert t-test to ARD
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams stats::t.test
#' @param by (`string`)\cr by column name
#' @param variable (`string`)\cr variable column name
#' @param ... passed to `t.test(...)`
#'
#' @return ARD data frame
#' @keywords internal
#' @examples
#' cardx:::.format_ttest_results(
#'   by = "ARM",
#'   variable = "AGE",
#'   paired = FALSE,
#'   lst_tidy =
#'     cards::eval_capture_conditions(
#'       stats::t.test(ADSL[["AGE"]] ~ ADSL[["ARM"]], paired = FALSE) |>
#'         broom::tidy()
#'     )
#' )
.format_ttest_results <- function(by, variable, lst_tidy, paired, ...) {
  # build ARD ------------------------------------------------------------------
  ret <-
    cards::tidy_as_ard(
      lst_tidy = lst_tidy,
      tidy_result_names = c("estimate", "estimate1", "estimate2", "statistic",
                            "p.value", "parameter", "conf.low", "conf.high",
                            "method", "alternative"),
      fun_args_to_record = c("mu", "paired", "var.equal", "conf.level"),
      formals = formals(asNamespace("stats")[["t.test.default"]]),
      passed_args = c(list(paired = paired), dots_list(...)),
      lst_ard_columns = list(group1 = by, variable = variable, context = "ttest")
    )

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_ttest_stat_labels(),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::tidy_ard_column_order()
}


#' Convert long paired data to wide
#'
#'
#' @param data (`data.frame`)\cr a data frame that is one line per subject per group
#' @param by (`string`)\cr by column name
#' @param variable (`string`)\cr variable column name
#' @param id (`string`)\cr subject id column name
#' @param env (`environment`) used for error messaging. Default is `rlang::caller_env()`
#'
#' @return a wide data frame
#' @keywords internal
#' @examples
#' cards::ADSL[c("ARM", "AGE")] |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
#'   dplyr::arrange(USUBJID, ARM) |>
#'   cardx:::.paired_data_pivot_wider(by = "ARM", variable = "AGE", id = "USUBJID")
.paired_data_pivot_wider <- function(data, by, variable, id, env = rlang::caller_env()) {
  # check the number of levels before pivoting data to wider format
  if (dplyr::n_distinct(data[[by]], na.rm = TRUE) != 2L) {
    cli::cli_abort("The {.arg by} argument must have two and only two levels.", call = env)
  }

  data |>
    # arrange data so the first group always appears first
    dplyr::arrange(.data[[by]]) |>
    tidyr::pivot_wider(
      id_cols = all_of(id),
      names_from = all_of(by),
      values_from = all_of(variable)
    ) |>
    stats::setNames(c(id, "by1", "by2"))
}

.df_ttest_stat_labels <- function() {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "estimate1", "Group 1 Mean",
    "estimate2", "Group 2 Mean",
    "estimate", "Mean Difference",
    "p.value", "p-value",
    "statistic", "t Statistic",
    "parameter", "Degrees of Freedom",
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "mu", "H0 Mean",
    "paired", "Paired t-test",
    "var.equal", "Equal Variances",
    "conf.level", "CI Confidence Level",
  )
}


