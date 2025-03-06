#' ARD Cochran-Mantel-Haenszel Chi-Squared Test
#'
#' @description
#' Analysis results data for Cochran-Mantel-Haenszel Chi-Squared Test for count data.
#' Calculated with `mantelhaen.test(x = data[[variable]], y = data[[by]], z = data[[strata]], ...)`.
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared.
#' @param strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to stratify by.
#' @param ... additional arguments passed to `stats::mantelhaen.test(...)`
#'
#' @return ARD data frame
#' @name ard_stats_mantelhaen_test
#' @export
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom"))
#' cards::ADSL |>
#'   ard_stats_mantelhaen_test(by = "ARM", variable = "AGEGR1", strata = "SEX")
ard_stats_mantelhaen_test <- cards::as_cards_fn(
  function(data, by, variable, strata, ...) {
    set_cli_abort_call()

    # check installed packages ---------------------------------------------------
    check_pkg_installed("broom")

    # check/process inputs -------------------------------------------------------
    check_not_missing(data)
    check_not_missing(variable)
    check_not_missing(by)
    check_not_missing(strata)
    cards::process_selectors(data, by = {{ by }}, variable = {{ variable }}, strata = {{ strata }})
    check_scalar(variable)
    check_scalar(by)
    check_scalar(strata)

    # return empty ARD if no variable selected ----------------------------------
    if (is_empty(variable)) {
      return(dplyr::tibble() |> cards::as_card())
    }

    formals_cmh <- formals(asNamespace("stats")[["mantelhaen.test"]])
    if (!"alternative" %in% names(dots_list())) formals_cmh$alternative <- "two.sided"

    # build ARD ------------------------------------------------------------------
    mantelhaen <- stats::mantelhaen.test(
      x = data[[variable]],
      y = data[[by]],
      z = data[[strata]],
      ...
    ) |>
      cards::eval_capture_conditions()

    result <- mantelhaen[["result"]] |>
      broom::tidy() |>
      dplyr::bind_cols(formals_cmh[-c(1:3)])

    dplyr::tibble(
      stat_name = names(result),
      stat = as.list(result),
      variable = variable,
      group1 = by,
      group2 = strata,
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "estimate" & result[["exact"]] ~ "Mantel-Haenszel Odds Ratio Estimate",
          .data$stat_name %in% "estimate" ~ "Conditional Maximum Likelihood Odds Ratio Estimate",
          .data$stat_name %in% "statistic" & result[["exact"]] ~ "Mantel-Haenszel X-squared Statistic",
          .data$stat_name %in% "statistic" ~ "Generalized Cochran-Mantel-Haenszel Statistic",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "parameter" ~ "Degrees of Freedom",
          .data$stat_name %in% "correct" ~ "Continuity Correction",
          .data$stat_name %in% "exact" ~ "Exact Conditional Test",
          .data$stat_name %in% "conf.level" ~ "CI Confidence Level",
          .data$stat_name %in% "conf.low" ~ "CI Lower Bound",
          .data$stat_name %in% "conf.high" ~ "CI Upper Bound",
          TRUE ~ .data$stat_name
        ),
      fmt_fn =
        map(
          .data$stat,
          function(.x) {
            if (is.integer(.x)) return(0L)
            if (is.numeric(.x)) return(1L)
            NULL
          }
        ),
      context = "stats_mantelhaen_test",
      warning = mantelhaen["warning"],
      error = mantelhaen["error"]
    ) |>
    cards::as_card() |>
    cards::tidy_ard_column_order()
  },
  stat_names = c(
    "estimate", "statistic", "p.value", "parameter", "correct", "exact", "conf.level", "conf.low", "conf.high"
  )
)
