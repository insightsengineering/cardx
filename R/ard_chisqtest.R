#' ARD Chi-squared Test
#'
#' @description
#' Analysis results data for Pearson's Chi-squared Test.
#' Calculated with `chisq.test(x = data[[variable]], y = data[[by]], ...)`
#'
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param by,variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to compare
#' @param ... additional arguments passed to `fisher.test(...)`
#'
#' @return ARD data frame
#' @export
#'
#' @examples
#' cards::ADSL |>
#'   ard_chisqtest(by = "ARM", variable = "AGEGR1")
ard_chisqtest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom.helpers", reference_pkg = "cards")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(by, "by")
  check_class_data_frame(x = data)
  cards::process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_scalar(by)
  check_scalar(variable)

  # build ARD ------------------------------------------------------------------
  cards::tidy_as_ard(
    lst_tidy =
      cards::eval_capture_conditions(
        stats::chisq.test(x = data[[variable]], y = data[[by]], ...) |>
          broom::tidy()
      ),
    tidy_result_names = c("statistic", "p.value", "parameter", "method"),
    fun_args_to_record =
      c("correct", "p", "rescale.p", "simulate.p.value", "B"),
    formals = formals(stats::chisq.test),
    passed_args = dots_list(...),
    lst_ard_columns = list(group1 = by, variable = variable, context = "chisqtest")
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "statistic" ~ "X-squared Statistic",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "parameter" ~ "Degrees of Freedom",
          TRUE ~ .data$stat_name,
        )
    )
}
