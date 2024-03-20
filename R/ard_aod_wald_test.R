#' ARD Wald Test
#'
#' @description
#' Function takes a regression model object and calculates Wald
#' statistical test using [`aod::wald.test`].
#'
#' @param x regression model object
#' @param ... arguments passed to `aod::wald.test(...)`
#'
#' @return data frame
#' @export
#'
#' @examplesIf cards::is_pkg_installed(c("aod"), reference_pkg = "cardx")
#' lm(AGE ~ ARM, data = cards::ADSL) |>
#'   ard_aod_wald_test()
ard_aod_wald_test <- function(x, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed(c("aod"), reference_pkg = "cardx")

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)

  # run aod::wald.test() -----------------------------------------------------------

  reg_model <- cards::eval_capture_conditions(
    ard_regression_basic(x, intercept = TRUE, stats_to_remove = c(
      "var_type",
      "var_label",
      "var_class", "label",
      "contrasts_type", "contrasts", "var_nlevels", "std.error",
      "conf.low", "conf.high", "statistic", "p.value", "estimate"
    ))
  )

  reg_model[["result"]] %>%
    dplyr::select(c(
      term = "variable",
      model_terms = "stat"
    )) %>%
    dplyr::mutate(term_id = dplyr::row_number()) %>%
    tidyr::nest(data = -"term") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      model_terms = unlist(.data$data[["model_terms"]]) %>%
        list(), model_terms_id = rlang::set_names(.data$data[["term_id"]]) %>%
        list(), wald_test = aod::wald.test(
        Sigma = stats::vcov(x),
        b = stats::coef(x), Terms = .data$model_terms_id
      ) %>%
        list(), df = .data$wald_test$result$chi2 %>% purrr::pluck("df"),
      statistic = .data$wald_test$result$chi2 %>% purrr::pluck("chi2"),
      p.value = .data$wald_test$result$chi2 %>% purrr::pluck("P"),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "term",
      "df", "statistic", "p.value"
    ) %>%
    tidyr::pivot_longer(
      cols = -"term",
      names_to = "stat_name",
      values_to = "stat"
    ) |>
    dplyr::mutate(
      stat = as.list(.data$stat),
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "statistic" ~ "Statistic",
          .data$stat_name %in% "df" ~ "Degrees of Freedom",
          .data$stat_name %in% "p.value" ~ "p-value",
          TRUE ~ .data$stat_name
        ),
      fmt_fn =
        map(
          .data$stat,
          function(.x) {
            # styler: off
            if (is.integer(.x)) return(0L)
            if (is.numeric(.x)) return(1L)
            # styler: on
            NULL
          }
        ),
      context = "aod_wald_test",
      warning = reg_model["warning"],
      error = reg_model["error"]
    ) %>%
    as.data.frame()
}
