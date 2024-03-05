#' ARD Kruskal-Wallis Test
#'
#' @description
#' Analysis results data for Kruskal-Wallis Rank Sum Test.
#'
#' Calculated with `kruskal.test(data[[variable]], data[[by]], ...)`
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf cards::is_pkg_installed("broom", reference_pkg = "cardx")
#' cards::ADSL |>
#'   ard_kruskaltest(by = "ARM", variable = "AGE")
ard_kruskaltest <- function(data, by, variable) {
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
        stats::kruskal.test(x = data[[variable]], g = data[[by]]) |>
          broom::tidy()
      ),
    tidy_result_names = c("statistic", "p.value", "parameter", "method"),
    lst_ard_columns = list(group1 = by, variable = variable, context = "kruskaltest")
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "statistic" ~ "Kruskal-Wallis chi-squared Statistic",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "parameter" ~ "Degrees of Freedom",
          TRUE ~ .data$stat_name,
        )
    )
}
