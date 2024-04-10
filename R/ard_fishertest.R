#' ARD Fisher's Exact Test
#'
#' @description
#' Analysis results data for Fisher's Exact Test.
#' Calculated with `fisher.test(x = data[[variable]], y = data[[by]], ...)`
#'
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to be compared. Independent tests will be computed for
#'   each variable.
#' @param ... additional arguments passed to `fisher.test(...)`
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))
#' cards::ADSL[1:30, ] |>
#'   ard_fishertest(by = "ARM", variables = "AGEGR1")
ard_fishertest <- function(data, by, variables, ...) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed(pkg = "broom", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(by)
  check_data_frame(data)
  cards::process_selectors(data, by = {{ by }}, variables = {{ variables }})
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
            stats::fisher.test(x = data[[variable]], y = data[[by]], ...) |>
              broom::tidy()
          ),
        tidy_result_names =
          c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
        fun_args_to_record =
          c(
            "workspace", "hybrid", "hybridPars", "control", "or",
            "conf.int", "conf.level", "simulate.p.value", "B"
          ),
        formals = formals(stats::fisher.test),
        passed_args = dots_list(...),
        lst_ard_columns = list(group1 = by, variable = variable, context = "fishertest")
      ) |>
        dplyr::mutate(
          .after = "stat_name",
          stat_label =
            dplyr::case_when(
              .data$stat_name %in% "p.value" ~ "p-value",
              TRUE ~ .data$stat_name,
            )
        )
    }
  ) |>
    dplyr::bind_rows()
}
