#' ARD Univariate Confidence Intervals
#'
#' `r lifecycle::badge('experimental')`\cr
#' Calculate confidence intervals for univariate statistics.
#'
#' @inheritParams cards::ard_continuous
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to include in summaries. Columns must be class `<logical>`
#'   or `<numeric>` values coded as `c(0, 1)`.
#' @param conf.level (`numeric`)\cr
#'   a scalar in `(0, 1)` indicating the confidence level.
#'   Default is `0.95`
#' @param method (`string`)\cr
#'   string indicating the type of confidence interval to calculate.
#'   Must be one of `r formals(ard_proportion_ci)[["method"]] |> eval() |> shQuote()`.
#'   See `?single_ci` for details.
#'
#' @return an ARD data frame
#' @export
#'
#' @examplesIf cards::is_pkg_installed("broom", reference_pkg = "cardx")
#' ard_single_ci(cards::ADSL, variables = c(AGE), method = "mean_with_T")
#' ard_single_ci(cards::ADSL, variables = c(AGE), method = "mean_with_Z")
ard_single_ci <- function(data,
                          variables,
                          by = dplyr::group_vars(data),
                          conf.level = 0.95,
                          method = c("mean_with_T", "mean_with_Z")) {
  # process inputs -------------------------------------------------------------
  cards::process_selectors(data, variables = {{ variables }}, by = {{ by }})
  method <- arg_match(method)
  if (method %in% c("mean")) {
    cards::process_selectors(data, strata = strata)
    check_scalar(strata)
  }

  # calculate confidence intervals ---------------------------------------------
  cards::ard_complex(
    data = data,
    variables = {{ variables }},
    by = {{ by }},
    statistic =
      ~ list(
        single_ci =
          switch(method,
                 "mean_with_T" = \(x, ...) single_ci_mean(x, conf.level = conf.level, use_t = TRUE),
                 "mean_with_Z" = \(x, ...) single_ci_mean(x, conf.level = conf.level, use_t = FALSE)
          )
      )
  ) |>
    dplyr::mutate(
      context = "single_ci"
    )
}
