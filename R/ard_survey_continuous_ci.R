#' ARD survey categorical CIs
#'
#' One-sample confidence intervals for continuous variables' means and medians.
#' Confidence limits are calculated with `survey::svymean()` and `survey::svyquantile()`.
#'
#'
#' @inheritParams ard_continuous.survey.design
#' @param method (`string`)\cr
#'   Method for confidence interval calculation.
#'   When `"svymean"`, the calculation is computed via `survey::svymean()`.
#'   Otherwise, it is calculated via`survey::svyquantile(interval.type=method)`
#' @param conf.level (scalar `numeric`)\cr
#'   confidence level for confidence interval. Default is `0.95`.
#' @param ... arguments passed to `survey::confint()`
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "survey", reference_pkg = "cardx"))
#' data(api, package = "survey")
#' dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#'
#' ard_survey_continuous_ci(dclus1, variables = api00)
#' ard_survey_continuous_ci(dclus1, variables = api00, method = "xlogit")
ard_survey_continuous_ci <- function(data,
                                      variables,
                                      by = NULL,
                                      method = c("svymean", "mean", "beta", "xlogit", "asin", "score"),
                                      conf.level = 0.95,
                                      ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_class(data, "survey.design")
  check_not_missing(variables)

  cards::process_selectors(
    data = data$variables,
    variables = {{ variables }},
    by = {{ by }}
  )
  check_scalar(by, allow_empty = TRUE)
  check_scalar_range(conf.level, range = c(0, 1))
  method <- arg_match(method)

  # calculate and return ARD of one sample CI ----------------------------------
  .calculate_ard_continuous_survey_ci(
    FUN = ifelse(method == "svymean", .svymean_confint_wrapper, .svyquantile_confint_wrapper),
    data = data,
    variables = variables,
    by = by,
    conf.level = conf.level,
    method = method,
    ...
  )
}

.calculate_ard_continuous_survey_ci <- function(FUN, data, variables, by, conf.level, ...) {
  # return empty data frame if no variables to process -------------------------
  if (is_empty(variables)) return(dplyr::tibble()) # styler: off

  # calculate results ----------------------------------------------------------
  map(
    variables,
    function(variable) {
      .calculate_one_ard_continuous_survey_ci(
        FUN = FUN,
        data = data,
        variable = variable,
        by = by,
        conf.level = conf.level,
        ...
      )
    }
  ) |>
    dplyr::bind_rows()
}

.calculate_one_ard_continuous_survey_ci <- function(FUN, data, variable, by, conf.level, ...) {
  if (!is_empty(by)) {
    by_levels <- .unique_values_sort(data$variables, variable = by)
    lst_data <-
      map(
        by_levels,
        ~call2("subset", expr(data), expr(!!sym(by) == !!.x)) |> eval()
      ) |>
      set_names(as.character(by_levels))
  }

  df_full <-
    case_switch(
      !is_empty(by) ~
        tidyr::expand_grid(
          group1_level = as.character(by_levels) |> as.list()
        ) |>
        dplyr::mutate(group1 = .env$by, variable = .env$variable),
      .default =
        dplyr::tibble(variable = .env$variable)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      lst_result =
        FUN(
          data =
            case_switch(
              is_empty(.env$by) ~ data,
              .default = lst_data[[.data$group1_level]]
            ),
          variable = .data$variable,
          conf.level = .env$conf.level,
          ...
        ) |>
        list(),
      result =
        .data$lst_result[["result"]]|>
        enframe("stat_name", "stat") |>
        list(),
      warning = .data$lst_result["warning"] |> unname(),
      error = .data$lst_result["error"] |> unname(),
      context = "survey_continuous_ci"
    ) |>
    dplyr::select(-"lst_result") |>
    dplyr::ungroup() |>
    tidyr::unnest("result") |>
    dplyr::mutate(
      stat_label = .data$stat_name,
      fmt_fn = map(.data$stat, ~case_switch(is.numeric(.x) ~ 2L, .default = as.character))
    ) |>
    cards::tidy_ard_column_order() %>%
    structure(., class = c("card", class(.)))
}

.svymean_confint_wrapper <- function(data, variable, conf.level, ...) {
  lst_results <-
    cards::eval_capture_conditions({
      svymean <-
        survey::svymean(x = reformulate2(variable), design = data, na.rm = TRUE)

      lst_svymean <- as.data.frame(svymean) |>
        as.list() |>
        set_names(c("estimate", "std.error"))

      lst_confint <- stats::confint(svymean, level = conf.level, ...) |>
        as.data.frame() |>
        as.list() |>
        set_names(c("conf.low", "conf.high"))

      c(lst_svymean, lst_confint)
    })

  # add NULL results if error
  if (is_empty(lst_results[["result"]])) {
    lst_results[["result"]] <- rep_named(c("estimate", "std.error", "conf.low", "conf.high"), list(NULL))
  }

  # add other args
  lst_results[["result"]] <- lst_results[["result"]] |> append(list(conf.level = conf.level))

  # return list result
  lst_results
}

.svyquantile_confint_wrapper <- function(data, variable, conf.level, method, ...) {
  lst_results <-
    cards::eval_capture_conditions({
      svyquantile <-
        survey::svyquantile(
          x = reformulate2(variable), design = data, quantiles = 0.5,
          na.rm = TRUE, interval.type = method
        )

      lst_svyquantile <- svyquantile |>
        getElement(1L) |>
        as.data.frame() |>
        dplyr::select(1L, last_col()) |>
        as.list() |>
        set_names(c("estimate", "std.error"))

      lst_confint <- stats::confint(svyquantile, level = conf.level, ...) |>
        as.data.frame() |>
        as.list() |>
        set_names(c("conf.low", "conf.high"))

      c(lst_svyquantile, lst_confint)
    })

  # add NULL results if error
  if (is_empty(lst_results[["result"]])) {
    lst_results[["result"]] <- rep_named(c("estimate", "std.error", "conf.low", "conf.high"), list(NULL))
  }

  # add other args
  lst_results[["result"]] <- lst_results[["result"]] |> append(list(conf.level = conf.level))

  # return list result
  lst_results
}

