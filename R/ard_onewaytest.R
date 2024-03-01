#' ARD One-way Test
#'
#' @description
#' Analysis results data for Testing Equal Means in a One-Way Layout.
#' Calculated with `oneway.test(data[[variable]] ~ as.factor(data[[by]]), data = data, ...)`
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared
#' @param ... additional arguments passed to `oneway.test(...)`
#'
#' @return ARD data frame
#' @export
#'
#' @examples
#' cards::ADSL |>
#'   ard_onewaytest(by = "ARM", variable = "AGE")
ard_onewaytest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(by)
  check_data_frame(data)
  cards::process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_scalar(by)
  check_scalar(variable)

  # build ARD ------------------------------------------------------------------
  cards::tidy_as_ard(
    lst_tidy =
      cards::eval_capture_conditions(
        stats::oneway.test(stats::reformulate(by, response = variable), data = data) |>
          broom::tidy()
      ),
    tidy_result_names = c("num.df", "den.df", "statistic", "p.value", "method"),
    fun_args_to_record =
      c("var.equal"),
    formals = formals(stats::oneway.test),
    passed_args = dots_list(...),
    lst_ard_columns = list(group1 = by, variable = variable, context = "Oneway.test")
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
