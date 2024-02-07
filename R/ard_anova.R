#' ARD ANOVA
#'
#' @description
#' Analysis results data for Analysis of Variance.
#' Calculated with `aov(data[[variable]] ~ as.factor(data[[by]]), ...)`
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param by,variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to compare
#' @param ... additional arguments passed to `aov(...)`
#'
#' @return ARD data frame
#' @export
#'
#' @examples
#' cards::ADSL |>
#'   ard_aov(by = "ARM", variable = "AGE")
ard_aov <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom", reference_pkg = "cardx")

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
        stats::aov(stats::reformulate(by, response=variable), data=data) |>
          broom::tidy() |>
          dplyr::slice_head()
      ),
    tidy_result_names = c("term", "df", "sumsq", "meansq", "statistic", "p.value"),
    fun_args_to_record =
      c("formula", "projections", "qr", "contrasts"),
    formals = formals(stats::aov),
    passed_args = dots_list(...),
    lst_ard_columns = list(group1 = by, variable = variable, context = "ANOVA")
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "term" ~ "Term",
          .data$stat_name %in% "sumsq" ~ "Sum of Squares",
          .data$stat_name %in% "meansq" ~ "Mean of Sum of Squares",
          .data$stat_name %in% "statistic" ~ "F Statistic",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "df" ~ "Degrees of Freedom",
          TRUE ~ .data$stat_name,
        )
    )
}
