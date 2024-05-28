#' ARD Proportion Confidence Intervals
#'
#' `r lifecycle::badge('experimental')`\cr
#' Calculate confidence intervals for proportions.
#'
#' @inheritParams cards::ard_categorical
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to include in summaries. Columns must be class `<logical>`
#'   or `<numeric>` values coded as `c(0, 1)`.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to stratify calculations by
#' @param conf.level (`numeric`)\cr
#'   a scalar in `(0, 1)` indicating the confidence level.
#'   Default is `0.95`
#' @param method (`string`)\cr
#'   string indicating the type of confidence interval to calculate.
#'   Must be one of `r formals(ard_proportion_ci)[["method"]] |> eval() |> shQuote("sh")`.
#'   See `?proportion_ci` for details.
#' @param strata,weights,max.iterations arguments passed to `proportion_ci_strat_wilson()`,
#'   when `method='strat_wilson'`
#' @param value ([`formula-list-selector`][syntax])\cr
#'   function will calculate the CIs for all levels of the variables specified.
#'   Use this argument to instead request only a single level by summarized.
#'   Default is `list(where(is_binary) ~ 1L, where(is.logical) ~ TRUE)`, where
#'   columns coded as `0`/`1` and `TRUE`/`FALSE` will summarize the `1` and `TRUE` levels.
#'
#' @return an ARD data frame
#' @export
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))
#' # compute CI for binary variables
#' ard_proportion_ci(mtcars, variables = c(vs, am), method = "wilson")
#'
#' # compute CIs for each level of a categorical variable
#' ard_proportion_ci(mtcars, variables = cyl, method = "jeffreys")
ard_proportion_ci <- function(data,
                              variables,
                              by = dplyr::group_vars(data),
                              method = c(
                                "waldcc", "wald", "clopper-pearson",
                                "wilson", "wilsoncc",
                                "strat_wilson", "strat_wilsoncc",
                                "agresti-coull", "jeffreys"
                              ),
                              conf.level = 0.95,
                              value = list(where(is_binary) ~ 1L, where(is.logical) ~ TRUE),
                              strata = NULL,
                              weights = NULL,
                              max.iterations = 10) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed(pkg = "broom", reference_pkg = "cardx")

  # process inputs -------------------------------------------------------------
  cards::process_selectors(data, variables = {{ variables }}, by = {{ by }})
  method <- arg_match(method)
  if (method %in% c("strat_wilson", "strat_wilsoncc")) {
    cards::process_selectors(data, strata = strata)
    check_scalar(strata)
  }
  cards::process_formula_selectors(
    data[variables],
    value = value
  )

  # calculate confidence intervals ---------------------------------------------
  map(
    variables,
    function(variable) {
      levels <- .unique_values_sort(data, variable = variable, value = value[[variable]])

      .calculate_ard_proportion(
        data = .as_dummy(data, variable = variable, levels = levels, by = by, strata = strata),
        variables = c(everything(), -all_of(c(by, strata))),
        by = all_of(by),
        method = method,
        conf.level = conf.level,
        strata = strata,
        weights = weights
      ) %>%
        # merge in the variable levels
        dplyr::left_join(
          dplyr::select(., "variable") |>
            dplyr::distinct() |>
            dplyr::mutate(variable_level = as.list(.env$levels)),
          by = "variable"
        ) |>
        # rename variable column
        dplyr::mutate(variable = .env$variable) |>
        dplyr::relocate("variable_level", .after = "variable")
    }
  ) |>
    dplyr::bind_rows()
}

.calculate_ard_proportion <- function(data, variables, by, method, conf.level, strata, weights, max.iterations) {
  cards::ard_complex(
    data = data,
    variables = {{ variables }},
    by = {{ by }},
    statistic =
      ~ list(
        prop_ci =
          switch(method,
            "waldcc" = \(x, ...) proportion_ci_wald(x, conf.level = conf.level, correct = TRUE),
            "wald" = \(x, ...) proportion_ci_wald(x, conf.level = conf.level, correct = FALSE),
            "wilsoncc" = \(x, ...) proportion_ci_wilson(x, conf.level = conf.level, correct = TRUE),
            "wilson" = \(x, ...) proportion_ci_wilson(x, conf.level = conf.level, correct = FALSE),
            "clopper-pearson" = \(x, ...) proportion_ci_clopper_pearson(x, conf.level = conf.level),
            "agresti-coull" = \(x, ...) proportion_ci_agresti_coull(x, conf.level = conf.level),
            "jeffreys" = \(x, ...) proportion_ci_jeffreys(x, conf.level = conf.level),
            "strat_wilsoncc" = \(x, data, ...) {
              proportion_ci_strat_wilson(x,
                strata = data[[strata]], weights = weights,
                max.iterations = max.iterations,
                conf.level = conf.level, correct = TRUE
              )
            },
            "strat_wilson" = \(x, data, ...) {
              proportion_ci_strat_wilson(x,
                strata = data[[strata]], weights = weights,
                max.iterations = max.iterations,
                conf.level = conf.level, correct = FALSE
              )
            }
          )
      )
  ) |>
    dplyr::mutate(
      context = "proportion_ci"
    )
}

.unique_values_sort <- function(data, variable, value = NULL) {
  unique_levels <-
    # styler: off
    if (is.logical(data[[variable]])) c(TRUE, FALSE)
    else if (is.factor(data[[variable]])) factor(levels(data[[variable]]), levels = levels(data[[variable]]))
    else unique(data[[variable]]) |> sort()
  # styler: on

  if (!is_empty(value) && !value %in% unique_levels) {
    cli::cli_warn(
      c("A value of {.code value={.val {value}}} for variable {.val {variable}}
         was passed, but is not one of the observed levels: {.val {unique_levels}}.",
        i = "This may be an error.",
        i = "If value is a valid, convert variable to factor with all levels specified to avoid this message."
      )
    )
  }
  if (!is_empty(value)) {
    unique_levels <- value
  }

  unique_levels
}

.as_dummy <- function(data, variable, levels, by, strata) {
  # define dummy variables and return tibble
  map(levels, ~ data[[variable]] == .x) |>
    set_names(paste0("this_is_not_a_column_name_anyone_would_choose_", variable, "_", levels, "...")) %>%
    {dplyr::tibble(!!!.)} |> # styler: off
    dplyr::bind_cols(data[c(by, strata)])
}
