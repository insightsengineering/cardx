#' ARD survey one-sample CIs
#'
#' @inheritParams ard_continuous.survey.design
#' @param method (`string`)\cr
#'   Method passed to `survey::svyciprop(method)`
#' @param conf.level (scalar `numeric`)\cr
#'   confidence level for confidence interval. Default is `0.95`.
#' @param ... arguments passed to `survey::svyciprop()`
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "survey", reference_pkg = "cardx"))
#' data(api, package = "survey")
#' dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#'
#' ard_survey_svyciprop(dclus1, variables = sch.wide)
#' ard_survey_svyciprop(dclus1, variables = sch.wide, method = "xlogit")
ard_survey_svyciprop <- function(data,
                                 variables,
                                 by = NULL,
                                 method = c("logit", "likelihood", "asin", "beta", "mean","xlogit"),
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

  # return empty data frame if no variables to process -------------------------
  if (is_empty(variables)) return(dplyr::tibble()) # styler: off

  # calculate results ----------------------------------------------------------
  map(
    variables,
    function(variable) {
      .calculate_one_ard_svyciprop(
        data = data,
        variable = variable,
        by = by,
        conf.level = conf.level,
        method = method,
        ...
      )
    }
  ) |>
    dplyr::bind_rows()
}

.calculate_one_ard_svyciprop <- function(data, variable, by, conf.level, method,
                                         expected_results = c("estimate", "conf.level", "conf.high"), ...) {
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
        .survey_fn_one_row(
          data =
            case_switch(
              is_empty(.env$by) ~ data,
              .default = lst_data[[.data$group1_level]]
            ),
          variable = .data$variable,
          variable_level = .data$variable_level,
          method = .env$method,
          conf.level = .env$conf.level,
          ...
        ) |>
        list(),
      result =
        case_switch(
          is_empty(.data$lst_result[["result"]]) ~ rep_named(expected_results, list(NULL)),
          .default = .data$lst_result[["result"]]
        ) |>
        append(list(method = method, conf.level = conf.level)) |>
        enframe("stat_name", "stat") |>
        list(),
      warning = .data$lst_result["warning"] |> unname(),
      error = .data$lst_result["error"] |> unname(),
      context = "survey_svyciprop"
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

.survey_fn_one_row <- function(data, variable, variable_level, method, conf.level, ...) {
  cards::eval_capture_conditions(
    survey::svyciprop(
      formula = inject(~I(!!sym(variable) == !!variable_level)),
      design = data,
      method = method,
      level = conf.level,
      ...
    ) %>%
      {list(.[[1]], attr(., "ci"))} |>
      unlist() |>
      set_names(c("estimate", "conf.low", "conf.high")) |>
      as.list()
  )
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
