#' ARD Survival Estimates
#'
#' @description
#' Analysis results data for survival quantiles and x-year survival estimates, extracted
#' from a [survival::survfit()] model.
#'
#' @param x ([survival::survfit()])\cr
#'   a [survival::survfit()] object. See below for details.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of the subject or participant ID
#' @param ... arguments passed to `t.test(...)`
#'
#' @return ARD data frame
#' @name ard_survest
#'
#' @details
#' For the `ard_survest()` function, the data is expected to be one row per subject.
#' The data is passed as `t.test(data[[variable]] ~ data[[by]], paired = FALSE, ...)`.
#'
#' For the `ard_paired_ttest()` function, the data is expected to be one row
#' per subject per by level. Before the t-test is calculated, the data are
#' reshaped to a wide format to be one row per subject.
#' The data are then passed as
#' `t.test(x = data_wide[[<by level 1>]], y = data_wide[[<by level 2>]], paired = TRUE, ...)`.
#'
#' @examplesIf broom.helpers::.assert_package("survival", pkg_search = "cardx", boolean = TRUE)
#' library(survival)
#'
#' survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   ard_survest()
NULL

#' @rdname ard_survest
#' @export
ard_survest <- function(x, by = NULL, times = NULL, probs = NULL, conf.level = 0.95, reverse = FALSE, ...) {
  # check installed packages ---------------------------------------------------
  browser()
  cards::check_pkg_installed("survival", reference_pkg = "cardx")

  # input checks ---------------------------------------------------------------
  if (purrr::every(x, ~ !inherits(.x, "survfit"))) {
    stop("Argument `x=` must be class 'survfit' created from the `survival::survfit()` function.",
         call. = FALSE
    )
  }
  if (c(is.null(times), is.null(probs)) %>% sum() != 1) {
    stop("One and only one of `times=` and `probs=` must be specified.", call. = FALSE)
  }
  if (reverse == TRUE && !is.null(probs)) {
    rlang::inform("`reverse=TRUE` argument ignored for survival quantile estimation.")
  }

  # check/process inputs -------------------------------------------------------
  check_not_missing(x)
  # check_not_missing(by)
  # check_class_data_frame(x = data)
  # data <- dplyr::ungroup(data)
  # cards::process_selectors(data, by = {{ by }}, variable = {{ variable }})
  # check_scalar(by)

  tidy <- broom::tidy(x)

  # adding time 0 to data frame
  tidy <-
    tidy %>%
    # making by a fct to preserve ordering
    mutate_at(vars(!!!by), ~ factor(., levels = unique(.))) %>%
    # if CI is missing, and SE is 0, making the CIs the estimate
    mutate_at(
      vars("conf.high", "conf.low"),
      ~ ifelse(is.na(.) & .data$std.error == 0, .data$estimate, .)
    ) %>%
    select(any_of(c("time", "estimate", "conf.high", "conf.low", "strata"))) %>%
    bind_rows(
      group_by(., !!!syms(by)) %>%
        slice(1) %>%
        mutate(
          time = 0,
          estimate = ifelse(multi_state, 0, 1),
          conf.low = ifelse(multi_state, 0, 1),
          conf.high = ifelse(multi_state, 0, 1)
        )
    ) %>%
    ungroup()

  # build ARD ------------------------------------------------------------------
  .format_survest_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions(
        stats::t.test(data[[variable]] ~ data[[by]], paired = FALSE, ...) |>
          broom::tidy()
      ),
    paired = FALSE,
    ...
  )
}

#' Convert t-test to ARD
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams stats::t.test
#' @param by (`string`)\cr by column name
#' @param variable (`string`)\cr variable column name
#' @param ... passed to `t.test(...)`
#'
#' @return ARD data frame
#' @keywords internal
#' @examples
#' cardx:::.format_survest_results(
#'   by = "ARM",
#'   variable = "AGE",
#'   paired = FALSE,
#'   lst_tidy =
#'     cards::eval_capture_conditions(
#'       stats::t.test(ADSL[["AGE"]] ~ ADSL[["ARM"]], paired = FALSE) |>
#'         broom::tidy()
#'     )
#' )
.format_survest_results <- function(by, variable, lst_tidy, paired, ...) {
  # build ARD ------------------------------------------------------------------
  ret <-
    cards::tidy_as_ard(
      lst_tidy = lst_tidy,
      tidy_result_names = c(
        "estimate", "estimate1", "estimate2", "statistic",
        "p.value", "parameter", "conf.low", "conf.high",
        "method", "alternative"
      ),
      fun_args_to_record = c("mu", "paired", "var.equal", "conf.level"),
      formals = formals(asNamespace("stats")[["t.test.default"]]),
      passed_args = c(list(paired = paired), dots_list(...)),
      lst_ard_columns = list(group1 = by, variable = variable, context = "survest")
    )

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_survest_stat_labels(),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::tidy_ard_column_order()
}

.df_survest_stat_labels <- function() {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "estimate1", "Group 1 Mean",
    "estimate2", "Group 2 Mean",
    "estimate", "Mean Difference",
    "p.value", "p-value",
    "statistic", "t Statistic",
    "parameter", "Degrees of Freedom",
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "mu", "H0 Mean",
    "paired", "Paired t-test",
    "var.equal", "Equal Variances",
    "conf.level", "CI Confidence Level",
  )
}
