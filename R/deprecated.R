#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge('deprecated')`\cr
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

# v0.3.0 -----------------------------------------------------------------------
#' @rdname deprecated
#' @export
ard_categorical_max <- function(...) {
  lifecycle::deprecate_soft(
    when = "0.3.0",
    what = "cardx::ard_categorical_max()",
    with = "cardx::ard_tabulate_max()"
  )

  ard_tabulate_max(...)
}

#' @importFrom cards ard_continuous
#' @export
cards::ard_continuous

#' @importFrom cards ard_categorical
#' @export
cards::ard_categorical

#' @importFrom cards ard_dichotomous
#' @export
cards::ard_dichotomous

#' @rdname deprecated
#' @export
ard_continuous.survey.design <- function(data, ...) {
  lifecycle::deprecate_soft(
    when = "0.3.0",
    what = "cardx::ard_continuous()",
    with = "cardx::ard_summary()"
  )

  ard_summary(data = data, ...) |>
    dplyr::mutate(context = "continuous")
}

#' @rdname deprecated
#' @export
ard_categorical.survey.design <- function(data, ...) {
  lifecycle::deprecate_soft(
    when = "0.3.0",
    what = "cardx::ard_categorical()",
    with = "cardx::ard_tabulate()"
  )

  ard_tabulate(data = data, ...) |>
    dplyr::mutate(context = "categorical")
}

#' @rdname deprecated
#' @export
# ard_dichotomous.survey.design <- function(data, variables, value = cards::maximum_variable_value(data[variables]), ...) {
#   lifecycle::deprecate_soft(
#     when = "0.3.0",
#     what = "cardx::ard_dichotomous()",
#     with = "cardx::ard_tabulate()",
#     details = "The `value` argument no longer has a default value and must be specified."
#   )
#
#   cards::process_selectors(data, variables = {{ variables }})
#   cards::process_formula_selectors(data[variables], value = value)
#   fill_formula_selectors(
#     data[variables],
#     value = formals(asNamespace("cards")[["ard_dichotomous.data.frame"]])[["value"]] |> eval()
#   )
#
#   ard_tabulate(
#     data = data,
#     variables = {{ variables }},
#     value = value,
#     ...
#   ) |>
#     dplyr::mutate(context = "dichotomous")
# }
