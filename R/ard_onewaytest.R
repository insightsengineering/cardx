#' ARD One-way Test
#'
#' @description
#' Analysis results data for Testing Equal Means in a One-Way Layout.
#' calculated with `oneway.test()`
#'
#' @inheritParams stats::oneway.test
#' @param ... additional arguments passed to `oneway.test(...)`
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf do.call(asNamespace("cards")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))
#' ard_onewaytest(AGE ~ ARM, data = cards::ADSL)
ard_onewaytest <- function(formula, data, ...) {
  # check installed packages ---------------------------------------------------
  do.call(asNamespace("cards")$check_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))

  # check/process inputs -------------------------------------------------------
  check_not_missing(formula)
  check_not_missing(data)
  check_data_frame(data)
  check_class(formula, cls = "formula")

  # build ARD ------------------------------------------------------------------

  cards::tidy_as_ard(
    lst_tidy =
      cards::eval_capture_conditions(
        stats::oneway.test(formula, data = data, ...) |>
          broom::tidy()
      ),
    tidy_result_names = c("num.df", "den.df", "statistic", "p.value", "method"),
    fun_args_to_record =
      c("var.equal"),
    formals = formals(stats::oneway.test),
    passed_args = dots_list(...),
    lst_ard_columns = list(context = "oneway.test")
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "num.df" ~ "Degrees of Freedom",
          .data$stat_name %in% "den.df" ~ "Denominator Degrees of Freedom",
          .data$stat_name %in% "statistic" ~ "F Statistic",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "method" ~ "Method",
          TRUE ~ .data$stat_name,
        )
    )
}
