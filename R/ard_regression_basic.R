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
#' @export
#'
#' @examples
#' lm(AGE ~ ARM, data = cards::ADSL) |>
#'   ard_regression_basic()
ard_regression_basic <- function(x, tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                                 stats_to_remove = c(
                                   "term", "var_type", "var_label", "var_class",
                                   "label", "contrasts_type", "contrasts", "var_nlevels"
                                 ),
                                 ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom.helpers", reference_pkg = "cards")

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
    dplyr::filter(!.data$stat_name %in% stats_to_remove)
}
