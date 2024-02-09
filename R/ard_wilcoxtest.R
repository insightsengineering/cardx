#' ARD Wilcoxon Rank-Sum Test
#'
#' @description
#' Analysis results data for paired and non-paired Wilcoxon Rank-Sum tests.
#'
#' @param data (`data.frame`)\cr
#'   a data frame. See below for details.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of the subject or participant ID
#' @param ... arguments passed to `wilcox.test(...)`
#'
#' @return ARD data frame
#' @name ard_wilcoxtest
#'
#' @details
#' For the `ard_wilcoxtest()` function, the data is expected to be one row per subject.
#' The data is passed as `wilcox.test(data[[variable]] ~ data[[by]], paired = FALSE, ...)`.
#'
#' For the `ard_paired_wilcoxtest()` function, the data is expected to be one row
#' per subject per by level. Before the t-test is calculated, the data are
#' reshaped to a wide format to be one row per subject.
#' The data are then passed as
#' `wilcox.test(x = data_wide[[<by level 1>]], y = data_wide[[<by level 2>]], paired = TRUE, ...)`.
#'
#' @examples
#' cards::ADSL |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   ard_wilcoxtest(by = "ARM", variable = "AGE")
#'
#' # constructing a paired data set,
#' # where patients receive both treatments
#' cards::ADSL[c("ARM", "AGE")] |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
#'   dplyr::arrange(USUBJID, ARM) |>
#'   ard_paired_wilcoxtest(by = ARM, variable = AGE, id = USUBJID)
NULL

#' @rdname ard_wilcoxtest
#' @export
ard_wilcoxtest <- function(data, by, variable, ...) {
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
  .format_wilcoxtest_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions(
        stats::wilcox.test(data[[variable]] ~ data[[by]], paired = FALSE, ...) |>
          broom::tidy()
      ),
    paired = FALSE,
    ...
  )
}

#' @rdname ard_wilcoxtest
#' @export
ard_paired_wilcoxtest <- function(data, by, variable, id, ...) {
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
  .format_wilcoxtest_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions({
        # adding this reshape inside the eval, so if there is an error it's captured in the ARD object
        data_wide <- .paired_data_pivot_wider(data, by = by, variable = variable, id = id)
        # perform paried t-test
        stats::wilcox.test(x = data_wide[["by1"]], y = data_wide[["by2"]], paired = TRUE, ...) |>
          broom::tidy()
      }),
    paired = TRUE,
    ...
  )
}

#' Convert t-test to ARD
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams stats::wilcox.test
#' @param by (`string`)\cr by column name
#' @param variable (`string`)\cr variable column name
#' @param ... passed to `wilcox.test(...)`
#'
#' @return ARD data frame
#' @keywords internal
#' @examples
#' cardx:::.format_wilcoxtest_results(
#'   by = "ARM",
#'   variable = "AGE",
#'   paired = FALSE,
#'   lst_tidy =
#'     cards::eval_capture_conditions(
#'       stats::wilcox.test(ADSL[["AGE"]] ~ ADSL[["ARM"]], paired = FALSE) |>
#'         broom::tidy()
#'     )
#' )
.format_wilcoxtest_results <- function(by, variable, lst_tidy, paired, ...) {
  # build ARD ------------------------------------------------------------------
  ret <-
    cards::tidy_as_ard(
      lst_tidy = lst_tidy,
      tidy_result_names = c("statistic", "p.value", "method", "alternative"),
      fun_args_to_record = c(
        "mu", "paired", "exact", "correct", "conf.int",
        "conf.level", "tol.root", "digits.rank"
      ),
      formals = formals(asNamespace("stats")[["wilcox.test.default"]]),
      passed_args = c(list(paired = paired), dots_list(...)),
      lst_ard_columns = list(group1 = by, variable = variable, context = "ttest")
    )

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_wilcoxtest_stat_labels(),
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

.df_wilcoxtest_stat_labels <- function() {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "statistic", "X-squared Statistic",
    "parameter", "Degrees of Freedom",
    "estimate", "Median of the Difference",
    "p.value", "p-value",
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "paired", "Paired test",
    "conf.level", "CI Confidence Level",
  )
}
