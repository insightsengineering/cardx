#' ARD Chi-squared Test
#'
#' @description
#' Analysis results data for Pearson's Chi-squared Test.
#' Calculated with `chisq.test(x = data[[variable]], y = data[[by]], ...)`
#'
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to be compared. Independent tests will be computed for
#'   each variable.
#' @param ... additional arguments passed to `chisq.test(...)`
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf cards::is_pkg_installed("broom", reference_pkg = "cardx")
#' cards::ADSL |>
#'   ard_chisqtest(by = "ARM", variables = "AGEGR1")
ard_chisqtest <- function(data, by, variables, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(by)
  check_data_frame(data)
  cards::process_selectors(data, by = {{ by }}, variables = {{ variables }})
  check_scalar(by)

  # if no variables selected, return empty tibble ------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }
  # build ARD ------------------------------------------------------------------
  lapply(
    variables,
    function(variable) {
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
  ) |>
    dplyr::bind_rows()
}
