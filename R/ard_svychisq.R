#' ARD Survey Chi-Square Test
#'
#' @description
#' Analysis results data for survey Chi-Square test using [`survey::svychisq()`].
#' Only two-way comparisons are supported.
#'
#' @param data (`survey.design`)\cr
#'   a survey design object often created with
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared
#' @param statistic (`character`)\cr
#'   statistic used to estimate chisq p-value.
#'   Default is the Rao-Scott second-order correction ("F"). See [`survey::svychisq`]
#'   for available statistics options.
#' @param ... arguments passed to [`survey::svychisq()`].
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf cards::is_pkg_installed(c("survey", "broom"), reference_pkg = "cardx")
#' data(api, package = "survey")
#' dclus2 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#'
#' ard_svychisq(dclus2, variable = sch.wide, by = comp.imp, statistic = "F")
ard_svychisq <- function(data, by, variable, statistic = "F", ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed(c("survey", "broom"), reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(by)
  check_class(data, cls = "survey.design")
  cards::process_selectors(data[["variables"]], by = {{ by }}, variable = {{ variable }})
  check_scalar(by)
  check_scalar(variable)

  # build ARD ------------------------------------------------------------------
  cards::tidy_as_ard(
    lst_tidy =
      cards::eval_capture_conditions(
        survey::svychisq(stats::reformulate(termlabels = paste(variable, by, sep = "+"), response = NULL), design = data, ...) |>
          broom::tidy()
      ),
    tidy_result_names = c("statistic", "p.value", "ndf", "ddf", "method"),
    formals = formals(survey::svyttest),
    passed_args = dots_list(...),
    lst_ard_columns = list(group1 = by, variable = variable, context = "svychisqtest")
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "statistic" ~ "X-squared Statistic",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "ndf" ~ "Nominator Degrees of Freedom",
          .data$stat_name %in% "ddf" ~ "Denominator Degrees of Freedom",
          .data$stat_name %in% "method" ~ "Stat calculation method",
          TRUE ~ .data$stat_name,
        )
    )
}
