#' ARD for LS Mean Difference
#'
#' @description
#' This function calculates least-squares mean differences using the 'emmeans'
#' package using the following
#'
#' ```r
#' emmeans::emmeans(object = <regression model>, specs = ~ <primary covariate>) |>
#'   emmeans::contrast(method = "pairwise") |>
#'   summary(infer = TRUE, level = <confidence level>)
#' ```
#'
#' The arguments `data`, `formula`, `method`, `method.args`, `package` are used
#' to construct the regression model via `cardx::construct_model()`.
#'
#' @param data (`data.frame`/`survey.design`)\cr
#'   a data frame or survey design object
#' @inheritParams construct_model
#' @param response_type (`string`)
#'   string indicating whether the model outcome is `'continuous'`
#'   or `'dichotomous'`. When `'dichotomous'`, the call to `emmeans::emmeans()` is
#'   supplemented with argument `regrid="response"`.
#' @param conf.level (scalar `numeric`)\cr
#'   confidence level for confidence interval. Default is `0.95`.
#' @param primary_covariate (`string`)\cr
#'   string indicating the primary covariate (typically the dichotomous treatment variable).
#'   Default is the first covariate listed in the formula.
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "emmeans"))
#' ard_emmeans_mean_difference(
#'   data = mtcars,
#'   formula = mpg ~ am + cyl,
#'   method = "lm"
#' )
#'
#' ard_emmeans_mean_difference(
#'   data = mtcars,
#'   formula = vs ~ am + mpg,
#'   method = "glm",
#'   method.args = list(family = binomial),
#'   response_type = "dichotomous"
#' )
ard_emmeans_mean_difference <- function(data, formula, method,
                                        method.args = list(),
                                        package = "base",
                                        response_type = c("continuous", "dichotomous"),
                                        conf.level = 0.95,
                                        primary_covariate =
                                          stats::terms(formula) |>
                                            attr("term.labels") |>
                                            getElement(1L)) {
  set_cli_abort_call()

  # check package installation -------------------------------------------------
  check_pkg_installed(c("emmeans", package))
  check_not_missing(data)
  check_not_missing(formula)
  check_not_missing(method)
  check_class(data, c("data.frame", "survey.design"))
  check_class(formula, cls = "formula")
  check_string(package)
  check_string(primary_covariate)
  check_scalar(conf.level)
  check_range(conf.level, range = c(0, 1))
  response_type <- arg_match(response_type, error_call = get_cli_abort_call())

  data_in <- if (dplyr::last(class(data)) == "survey.design") data$variables else data

  # build ARD ------------------------------------------------------------------
  cards::ard_complex(
    data = data_in,
    variables = all_of(primary_covariate),
    statistic = all_of(primary_covariate) ~ list(
      emmeans =
        .calc_emmeans_mean_difference(
          data, formula, method, {{ method.args }}, package, response_type, conf.level, primary_covariate
        )
    )
  ) |>
    dplyr::select(-"stat_label") |>
    dplyr::left_join(
      .df_ttest_stat_labels(primary_covariate),
      by = "stat_name"
    ) |>
    dplyr::mutate(
      variable = "contrast",
      variable_level = if ("variable_level" %in% .data$stat_name) {
        .data$stat[.data$stat_name == "variable_level"]
      } else {
        NA
      },
      group1 = .env$primary_covariate,
      stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name),
      context = "emmeans_mean_difference",
    ) |>
    dplyr::filter(.data$stat_name != "variable_level") |>
    cards::as_card() |>
    cards::tidy_ard_column_order() |>
    cards::tidy_ard_row_order()
}

# function to perform calculations -------------------------------------------
.calc_emmeans_mean_difference <- function(data, formula, method,
                                          method.args,
                                          package,
                                          response_type,
                                          conf.level,
                                          primary_covariate) {
  cards::as_cards_fn(
    \(x, ...) {
      # construct primary model ------------------------------------------------
      mod <-
        construct_model(
          data = data, formula = formula, method = method,
          method.args = {{ method.args }},
          package = package, env = caller_env()
        )

      # emmeans ----------------------------------------------------------------
      emmeans_args <- list(object = mod, specs = reformulate2(primary_covariate))
      if (response_type %in% "dichotomous") emmeans_args <- c(emmeans_args, list(regrid = "response"))
      emmeans <-
        withr::with_namespace(
          package = "emmeans",
          code = do.call("emmeans", args = emmeans_args)
        )

      df_results <-
        emmeans |>
        emmeans::contrast(method = "pairwise") |>
        summary(infer = TRUE, level = conf.level)

      # convert results to ARD format ------------------------------------------
      df_results |>
        dplyr::as_tibble() |>
        dplyr::rename(
          conf.low = any_of("asymp.LCL"),
          conf.high = any_of("asymp.UCL"),
          conf.low = any_of("lower.CL"),
          conf.high = any_of("upper.CL")
        ) %>%
        dplyr::select(
          variable_level = "contrast",
          "estimate",
          std.error = "SE", "df",
          "conf.low", "conf.high", "p.value"
        ) %>%
        dplyr::mutate(
          conf.level = .env$conf.level,
          method =
            ifelse(
              length(attr(stats::terms(formula), "term.labels") |> discard(~ startsWith(., "1 |"))) == 1L,
              "Least-squares mean difference",
              "Least-squares adjusted mean difference"
            )
        )
    },
    stat_names = c("estimate", "std.error", "df", "conf.low", "conf.high", "p.value", "conf.level", "method")
  )
}
