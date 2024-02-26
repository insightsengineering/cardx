#' Regression ARD
#'
#' Function takes a regression model object and converts it to a ARD
#' structure using the `broom.helpers` package.
#'
#' @param x regression model object
#' @param tidy_fun (`function`)\cr
#'   a tidier. Default is [`broom.helpers::tidy_with_broom_or_parameters`]
#' @param ... Arguments passed to `broom.helpers::tidy_plus_plus()`
#'
#' @return data frame
#' @name ard_regression
#'
#' @examples
#' lm(AGE ~ ARM, data = cards::ADSL) |>
#'   ard_regression(add_estimate_to_reference_rows = TRUE)
NULL

#' @rdname ard_regression
#' @export
ard_regression <- function(x, ...) {
  UseMethod("ard_regression")
}

#' @rdname ard_regression
#' @export
ard_regression.default <- function(x, tidy_fun = broom.helpers::tidy_with_broom_or_parameters, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom.helpers", reference_pkg = "cards")

  # check inputs ---------------------------------------------------------------
  check_not_missing(x, "model")

  # summarize model ------------------------------------------------------------
  broom.helpers::tidy_plus_plus(
    model = x,
    tidy_fun = tidy_fun,
    ...
  ) |>
    dplyr::mutate(
      variable_level = dplyr::if_else(.data$var_type %in% "continuous", NA_character_, .data$label),
      dplyr::across(-c("variable", "variable_level"), .fns = as.list)
    ) |>
    tidyr::pivot_longer(
      cols = -c("variable", "variable_level"),
      names_to = "stat_name",
      values_to = "stat"
    ) |>
    dplyr::filter(map_lgl(.data$stat, Negate(is.na))) |>
    dplyr::mutate(
      fmt_fn =
        lapply(
          .data$stat,
          function(x) {
            switch(is.integer(x), 0L) %||% # styler: off
              switch(is.numeric(x), 1L) # styler: off
          }
        ),
      context = "regression",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "var_label" ~ "Label",
          .data$stat_name %in% "var_class" ~ "Class",
          .data$stat_name %in% "var_type" ~ "Type",
          .data$stat_name %in% "var_nlevels" ~ "N Levels",
          .data$stat_name %in% "contrasts_type" ~ "Contrast Type",
          .data$stat_name %in% "label" ~ "Level Label",
          .data$stat_name %in% "n_obs" ~ "N Obs.",
          .data$stat_name %in% "n_event" ~ "N Events",
          .data$stat_name %in% "exposure" ~ "Exposure Time",
          .data$stat_name %in% "estimate" ~ "Coefficient",
          .data$stat_name %in% "std.error" ~ "Standard Error",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "conf.low" ~ "CI Lower Bound",
          .data$stat_name %in% "conf.high" ~ "CI Upper Bound",
          TRUE ~ .data$stat_name
        )
    ) |>
    cards::tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))} # styler: off
}

#' ard_regression_basic
#'
#' @description
#' A function that takes a regression model and provides basic statistics in an
#' ARD structure.
#'
#'
#' @param x regression model object
#' @param tidy_fun(`function`)\cr
#'   a tidier. Default is [`broom.helpers::tidy_with_broom_or_parameters`]
#' @param ... Arguments passed to `broom.helpers::tidy_plus_plus()`
#'
#' @return data frame
#' @name ard_regression_basic
#' @export
#'
#' @examples
#' lm(AGE ~ ARM, data = cards::ADSL) |>
#'   ard_regression_basic()
#'
ard_regression_basic <- function(x, tidy_fun = broom.helpers::tidy_with_broom_or_parameters, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom.helpers", reference_pkg = "cards")

  # check inputs ---------------------------------------------------------------
  check_not_missing(x, "model")

  args <-
    list(
      add_reference_rows = FALSE,
      add_estimate_to_reference_rows = FALSE,
      add_n = FALSE,
      intercept = FALSE
    ) |>
    utils::modifyList(val = rlang::dots_list(...))

  rlang::inject(ard_regression(x = x, tidy_fun = tidy_fun, !!!args)) |>
    dplyr::filter(!.data$stat_name %in% c("term", "var_type", "var_label", "var_class", "label"))
}
