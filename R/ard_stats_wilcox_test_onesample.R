#' ARD one-sample Wilcox Rank-sum
#'
#' @description
#' Analysis results data for one-sample Wilcox Rank-sum.
#' Result may be stratified by including the `by` argument.
#'
#' @param data (`data.frame`)\cr
#'   a data frame. See below for details.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to be analyzed. Independent Wilcox Rank-sum tests will be computed for
#'   each variable.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   optional column name to stratify results by.
#' @inheritParams ard_stats_wilcox_test
#'
#' @return ARD data frame
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))
#' cards::ADSL |>
#'   ard_stats_wilcox_test_onesample(by = ARM, variables = AGE)
ard_stats_wilcox_test_onesample <- function(data, variables, by = dplyr::group_vars(data), conf.level = 0.95, ...) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed("broom", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_data_frame(data)
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{ by }}, variables = {{ variables }})
  check_scalar_range(conf.level, range = c(0, 1))

  # if no variables selected, return empty tibble ------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }

  cards::ard_continuous(
    data = data,
    variables = all_of(variables),
    by = all_of(by),
    statistic = all_of(variables) ~ list(t_test_onesample = \(x) stats::wilcox.test(x = x, conf.level = conf.level, ...) |> broom::tidy())
  ) |>
    cards::bind_ard(
      cards::ard_continuous(
        data = data,
        variables = all_of(variables),
        by = all_of(by),
        statistic =
          all_of(variables) ~
          list(conf.level = \(x) {
            formals(asNamespace("stats")[["wilcox.test.default"]])[c("mu", "exact", "conf.int", "tol.root", "digits.rank")] |>
              utils::modifyList(list(conf.level = conf.level, ...)) |>
              compact()
          })
      )
    ) |>
    dplyr::select(-"stat_label") |>
    dplyr::left_join(
      .df_ttest_stat_labels(by = NULL),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::tidy_ard_row_order() |>
    cards::tidy_ard_column_order()
}
