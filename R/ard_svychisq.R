#' ARD Survey Chi-Square Test
#'
#' @description
#' Analysis results data for survey Chi-Square test using [`survey::svychisq()`].
#' Only two-way comparisons are supported.
#'
#' @param data (`survey.design`)\cr
#'   a survey design object often created with the {survey} package
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to be compared. Independent tests will be computed for
#'   each variable.
#' @param statistic (`character`)\cr
#'   statistic used to estimate Chisq p-value.
#'   Default is the Rao-Scott second-order correction ("F"). See [`survey::svychisq`]
#'   for available statistics options.
#' @param ... arguments passed to [`survey::svychisq()`].
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf cards::is_pkg_installed(c("survey", "broom"), reference_pkg = "cardx")
#' data(api, package = "survey")
#' dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#'
#' ard_svychisq(dclus1, variables = sch.wide, by = comp.imp, statistic = "F")
ard_svychisq <- function(data, by, variables, statistic = "F", ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed(c("survey", "broom"), reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(by)
  check_class(data, cls = "survey.design")
  cards::process_selectors(data[["variables"]], by = {{ by }}, variables = {{ variables }})
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
            survey::svychisq(stats::reformulate(termlabels = paste(variable, by, sep = "+"), response = NULL), design = data, statistic = statistic, ...) |>
              broom::tidy()
          ),
        tidy_result_names = c("statistic", "p.value", "ndf", "ddf", "method"),
        passed_args = dots_list(...),
        lst_ard_columns = list(group1 = by, variable = variable, context = "svychisq")
      ) |>
        dplyr::mutate(
          .after = "stat_name",
          stat_label =
            dplyr::case_when(
              .data$stat_name %in% "statistic" ~ "Statistic",
              .data$stat_name %in% "p.value" ~ "p-value",
              .data$stat_name %in% "ndf" ~ "Nominator Degrees of Freedom",
              .data$stat_name %in% "ddf" ~ "Denominator Degrees of Freedom",
              TRUE ~ .data$stat_name,
            )
        )
    }
  ) |>
    dplyr::bind_rows()
}
