

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

  if (is_empty(variables)) return(dplyr::tibble()) # styler: off

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

.calculate_one_ard_svyciprop <- function(data, variable, by, conf.level, method, ...) {
  variable_levels <- .unique_values_sort(data$variables, variable = variable)
  if (!is_empty(by)) by_levels <- .unique_values_sort(data$variables, variable = by) # styler: off

  browser()
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
      data =
        case_switch(
          is_empty(.env$by) ~ data,
          .default = inject(subset(data, !!.data$group1 == !!.data$group1_level))
        ) |>
        list(),
      # lst_result =
      #   .survey_fn_one_row(
      #     data =
      #       case_switch(
      #         is_empty(.env$by) ~ data,
      #         .default = inject(subset(data, !!.data$group1 == !!.data$group1_level))
      #       ),
      #     variable = .data$variable,
      #     variable_level = .data$variable_level,
      #     method = .env$method,
      #     conf.level = .env$conf.level,
      #     ...
      #   ) |>
      #   list()
    )
}

.survey_fn_one_row <- function(data, variable, variable_level, method, conf.level, ...) {
  browser()
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
