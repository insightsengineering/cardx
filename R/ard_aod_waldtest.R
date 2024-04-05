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
#'   ard_aod_waldtest()
ard_aod_waldtest <- function(x, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed(c("aod"), reference_pkg = "cardx")

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)

  # run regression() -----------------------------------------------------------
  reg_model <- cards::eval_capture_conditions(
    ard_regression_basic(x, intercept = TRUE, stats_to_remove = c(
      "var_type",
      "var_label",
      "var_class", "label",
      "contrasts_type", "contrasts", "var_nlevels", "std.error",
      "conf.low", "conf.high", "statistic", "p.value", "estimate"
    ))
  )

  if (!is.null(reg_model[["error"]])) {
    cards::tidy_as_ard(
      lst_tidy = reg_model,
      tidy_result_names = c("statistic", "p.value", "Degrees of Freedom"),
      lst_ard_columns = list(context = "aod_wald_test")
    )
  } else {
    aod <-
      reg_model[["result"]] %>%
      dplyr::select(c(
        variable = "variable",
        model_terms = "stat"
      )) %>%
      dplyr::mutate(term_id = dplyr::row_number()) %>%
      tidyr::nest(data = -"variable") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        model_terms = unlist(.data$data[["model_terms"]]) %>% list(),
        model_terms_id = rlang::set_names(.data$data[["term_id"]]) %>% list()
      )
    # run wald.test() -----------------------------------------------------------
    wald_test <-
      cards::eval_capture_conditions(lapply(seq_along(1:length(aod$model_terms_id)), function(terms_id) {
        aod::wald.test(
          Sigma = stats::vcov(x),
          b = stats::coef(x), Terms = aod$model_terms_id[[terms_id]]
        )
      }))

    extract_wald_results <- function(wald_test) {
      df <- wald_test$result$chi2[("df")]
      statistic <- wald_test$result$chi2[("chi2")]
      p.value <- wald_test$result$chi2[("P")]
      data.frame(df, statistic, p.value)
    }

    df_list <- do.call(rbind, lapply(wald_test$result, extract_wald_results))

    cbind(aod$variable, df_list) %>%
      tidyr::pivot_longer(
        cols = !"aod$variable",
        names_to = "stat_name",
        values_to = "stat"
      ) %>%
      dplyr::rename(
        "variable" = "aod$variable"
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
        warning = wald_test["warning"],
        error = wald_test["error"]
      ) |>
      cards::tidy_ard_column_order() %>%
      {
        structure(., class = c("card", class(.)))
      }
  }
}
