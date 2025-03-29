#' Basic Regression ARD
#'
#' @description
#' A function that takes a regression model and provides basic statistics in an
#' ARD structure.
#' The default output is simpler than [`ard_regression()`].
#' The function primarily matches regression terms to underlying variable names
#' and levels.
#' The default arguments used are
#'
#' ```r
#' broom.helpers::tidy_plus_plus(
#'   add_reference_rows = FALSE,
#'   add_estimate_to_reference_rows = FALSE,
#'   add_n = FALSE,
#'   intercept = FALSE
#' )
#' ```
#'
#' @inheritParams ard_regression
#' @param stats_to_remove (`character`)\cr
#'   character vector of statistic names to remove. Default is
#'   `c("term", "var_type", "var_label", "var_class", "label", "contrasts_type", "contrasts", "var_nlevels")`.
#'
#' @return data frame
#' @name ard_regression_basic
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom.helpers"))
#' lm(AGE ~ ARM, data = cards::ADSL) |>
#'   ard_regression_basic()
#'
#' ard_regression_basic(
#'   x = cards::ADSL,
#'   formula = AGE ~ ARM,
#'   method = "lm"
#' )
NULL

#' @rdname ard_regression_basic
#' @export
ard_regression_basic <- function(x, ...) {
  UseMethod("ard_regression_basic")
}

#' @rdname ard_regression_basic
#' @export
ard_regression_basic.default <- function(x, tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                                         stats_to_remove = c(
                                           "term", "var_type", "var_label", "var_class",
                                           "label", "contrasts_type", "contrasts", "var_nlevels"
                                         ),
                                         ...) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed(pkg = "broom.helpers")

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_class(stats_to_remove, cls = "character", allow_empty = TRUE)
  if (is_empty(stats_to_remove)) stats_to_remove <- character(0L) # styler: off

  args <-
    list(
      add_reference_rows = FALSE,
      add_estimate_to_reference_rows = FALSE,
      add_n = FALSE,
      intercept = FALSE
    ) |>
    utils::modifyList(val = rlang::dots_list(...))

  rlang::inject(ard_regression(x = x, tidy_fun = tidy_fun, !!!args)) |>
    dplyr::filter(!.data$stat_name %in% .env$stats_to_remove) |>
    dplyr::select(-(cards::all_ard_variables("levels") & dplyr::where(\(x) all(is.na(x)))))
}

#' @rdname ard_regression_basic
#' @export
ard_regression_basic.data.frame <- function(x, formula, method, method.args = list(), package = "base",
                                            tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                                            stats_to_remove = c(
                                              "term", "var_type", "var_label", "var_class",
                                              "label", "contrasts_type", "contrasts", "var_nlevels"
                                            ),
                                            ...) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(x)
  check_not_missing(formula)
  check_not_missing(method)
  check_class(formula, cls = "formula")

  # build model ----------------------------------------------------------------
  model <-
    construct_model(
      data = x,
      formula = formula,
      method = method,
      method.args = {{ method.args }},
      package = package
    )

  # summarize model ------------------------------------------------------------
  ard_regression_basic(x = model, tidy_fun = tidy_fun, stats_to_remove = stats_to_remove, ...)
}
