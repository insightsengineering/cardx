#' ARD survey categorical CIs
#'
#' Confidence intervals for categorical variables calculated via
#' [`survey::svyciprop()`].
#'
#' @inheritParams ard_continuous.survey.design
#' @param method (`string`)\cr
#'   Method passed to `survey::svyciprop(method)`
#' @param conf.level (scalar `numeric`)\cr
#'   confidence level for confidence interval. Default is `0.95`.
#' @param df (`numeric`)\cr
#'   denominator degrees of freedom, passed to `survey::svyciprop(df)`.
#'   Default is `survey::degf(data)`.
#' @param ... arguments passed to `survey::svyciprop()`
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "survey", reference_pkg = "cardx"))
#' data(api, package = "survey")
#' dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#'
#' ard_survey_categorical_ci(dclus1, variables = sch.wide)
#' ard_survey_categorical_ci(dclus1, variables = sch.wide, method = "xlogit")
ard_survey_categorical_ci <- function(data,
                                      variables,
                                      by = NULL,
                                      method = c("logit", "likelihood", "asin", "beta", "mean","xlogit"),
                                      conf.level = 0.95,
                                      df = survey::degf(data),
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
  .calculate_ard_onesample_survey_ci(
    FUN = .svyciprop_wrapper,
    data = data,
    variables = variables,
    by = by,
    conf.level = conf.level,
    method = method,
    df = df,
    ...
  )
}

.calculate_ard_onesample_survey_ci <- function(FUN, data, variables, by, conf.level, ...) {
  # return empty data frame if no variables to process -------------------------
  if (is_empty(variables)) return(dplyr::tibble()) # styler: off

  # calculate results ----------------------------------------------------------
  map(
    variables,
    function(variable) {
      .calculate_one_ard_categorical_survey_ci(
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

.calculate_one_ard_categorical_survey_ci <- function(FUN, data, variable, by, conf.level, ...) {
  variable_levels <- .unique_values_sort(data$variables, variable = variable)
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
          group1_level = as.character(by_levels) |> as.list(),
          variable_level = as.character(variable_levels) |> as.list()
        ) |>
        dplyr::mutate(group1 = .env$by, variable = .env$variable),
      .default =
        dplyr::tibble(variable = .env$variable,
                      variable_level = as.character(variable_levels) |> as.list())
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
          variable_level = .data$variable_level,
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
      context = "survey_categorical_ci"
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


.svyciprop_wrapper <- function(data, variable, variable_level, conf.level, method, df, ...) {
  lst_results <-
    cards::eval_capture_conditions(
      survey::svyciprop(
        formula = inject(~I(!!sym(variable) == !!variable_level)),
        design = data,
        method = method,
        level = conf.level,
        df = df,
        ...
      ) %>%
        {list(.[[1]], attr(., "ci"))} |>
        unlist() |>
        set_names(c("estimate", "conf.low", "conf.high")) |>
        as.list()
    )

  # add NULL results if error
  if (is_empty(lst_results[["result"]])) {
    lst_results[["result"]] <- rep_named(c("estimate", "conf.low", "conf.high"), list(NULL))
  }

  # add other args
  lst_results[["result"]] <- lst_results[["result"]] |> append(list(method = method, conf.level = conf.level))

  # return list result
  lst_results
}


case_switch <- function(..., .default = NULL) {
  dots <- dots_list(...)

  for (f in dots) {
    if (isTRUE(eval(f_lhs(f), envir = attr(f, ".Environment")))) {
      return(eval(f_rhs(f), envir = attr(f, ".Environment")))
    }
  }

  return(.default)
}
