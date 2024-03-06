

ard_svy_continuous <- function(data, variables, by = NULL,
                               statistic = everything() ~ c("median", "p25", "p75"),
                               fmt_fn = NULL, stat_label = NULL) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_class(data, cls = "survey.design")
  check_not_missing(variables)

  # process inputs -------------------------------------------------------------
  cards::process_selectors(data$variables, variables = {{ variables }}, by = {{ by }})
  variables <- setdiff(variables, by)
  cards::process_formula_selectors(data$variables[variables], statistic = statistic)
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) all(x %in% accepted_svy_stats()),
    error_msg = c("Error in the values of the {.arg statistic} argument.",
                  i = "Values must be in {.val {accepted_svy_stats(FALSE)}}")
  )

  # compute the weighted statistics --------------------------------------------
  map(
    names(statistic),
    function(variable) {
      map(
        statistic[[variable]],
        function(statistic) {
          .compute_svy_stat(data, variable = variable, by = by, stat_name = statistic)
        }
      )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      context = "continuous",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% c("mean", "median", "sum") ~ tools::toTitleCase(.data$stat_name),
          .data$stat_name %in% "min" ~ "Minimum",
          .data$stat_name %in% "max" ~ "Maximum",
          .data$stat_name %in% "deff" ~ "Design Effect",
          .data$stat_name %in% "mean.std.error" ~ "SE(Mean)",
          .data$stat_name %in% paste0("p", 0:100) ~
            paste0(substr(.data$stat_name, 2, nchar(.data$stat_name)), "% Percentile"),
          TRUE ~ .data$stat_name
        )
    ) |>
    cards::tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))} # styler: off

}

accepted_svy_stats <- function(expand_quantiles = TRUE) {
  base_stats <- c("mean", "median", "min", "max")
  if (expand_quantiles){
    return(c(base_stats, paste0("p", 0:100)))
  }
  c(base_stats, "p##")
}


.compute_svy_stat <- function(data, variable, by = NULL, stat_name) {
  # difftime variable needs to be transformed into numeric for svyquantile
  if (inherits(data$variables[[variable]], "difftime")) {
    data$variables[[variable]] <- unclass(data$variables[[variable]])
  }

  # styler: off
  if (stat_name %in% "mean") args <- list(FUN = survey::svymean)
  else if (stat_name %in% "sum") args <- list(FUN = survey::svytotal)
  else if (stat_name %in% "var") args <- list(FUN = survey::svyvar)
  else if (stat_name %in% "sd") args <- list(FUN = \(...) survey::svyvar(...) |> sqrt())
  else if (stat_name %in% "mean.std.error") args <- list(FUN = \(...) survey::svymean(...) |> survey::SE())
  else if (stat_name %in% "deff") args <- list(FUN = \(...) survey::svymean(..., deff = TRUE) |> survey::deff())
  else if (stat_name %in% "min") args <- list(FUN = \(x, design, ...) min(design$variables[[all.vars(x)]], na.rm = na.rm))
  else if (stat_name %in% "max") args <- list(FUN = \(x, design, ...) max(design$variables[[all.vars(x)]], na.rm = na.rm))
  else if (stat_name %in% c("median", paste0("p", 0:100))) {
    quantile <- ifelse(stat_name %in% "median", 0.5, substr(stat_name, 2, nchar(stat_name)) |> as.numeric())
    args <-
      if (is_empty(by)) list(FUN = \(...) survey::svyquantile(...)[[1]], quantiles = quantile)
      else list(FUN = \(...) survey::svyquantile(...), quantiles = quantile)
  }
  # styler: on

  # adding additional args to pass
  args <-
    args |>
    append(
      list(
        design = data,
        # if all values are NA, turn na.rm to FALSE to avoid error
        na.rm = !all(is.na(data$variables[[variable]])),
        keep.var = FALSE
      )
    )


  # if no by variable, calculate univariate statistics
  if (is_empty(by)) {
    args$x <- reformulate(variable)
    # calculate statistic (and remove FUN from the argument list)
    stat <-
      cards::eval_capture_conditions(
        do.call(args$FUN, args = args |> utils::modifyList(list(FUN = NULL)))
      )
    # if the result was calculated, then put it into a tibble
    if (!is.null(stat[["result"]])) {
      df_stat <-
        dplyr::tibble(variable, stat[["result"]][1]) |>
        set_names(c("variable", "stat")) |>
        dplyr::mutate(
          stat = as.list(unname(.data$stat)),
          warning = list(stat[["warning"]]),
          error = list(stat[["error"]])
        )
    }
    # otherwise, if there was an error return tibble with error message
    else {
      df_stat <-
        dplyr::tibble(
          variable = .env$variable,
          stat = list(NULL),
          warning = list(.env$stat[["warning"]]),
          error = list(.env$stat[["error"]])
        )
    }
  }
  # if there is by variable(s), calculate statistics for the combinations
  else {
    args$formula <- reformulate(variable)
    args$by <- reformulate(by)
    stat <-
      if (stat_name %in% c("median", paste0("p", 0:100))) {
        cards::eval_capture_conditions(
          do.call(survey::svyby, args) |> set_names(c(by, "quantile", "ci.2.5", "ci.97.5", "se"))
        )
      }
      else cards::eval_capture_conditions(do.call(survey::svyby, args))

    # if the result was calculated, then put it into a tibble
    if (!is.null(stat[["result"]])) {
      df_stat <- stat[["result"]][seq_len(length(by) + 1L)] |>
        dplyr::as_tibble() |>
        set_names(paste0("group", seq_along(by), "_level"), "stat") |>
        dplyr::bind_cols(
          dplyr::tibble(!!!c(by, variable)) |>
            set_names(paste0("group", seq_along(by)), "variable")
        ) |>
        dplyr::mutate(
          dplyr::across(c(cards::all_ard_groups("levels"), "stat"), as.list),
          warning = list(stat[["warning"]]),
          error = list(stat[["error"]])
        )
    }
    # otherwise, if there was an error return tibble with error message
    else {
      df_stat <-
        cards::nest_for_ard(data$variables, by = by, key = "...ard_no_one_will_ever_pick_this...") |>
        dplyr::select(-"...ard_no_one_will_ever_pick_this...") |>
        dplyr::mutate(
          variable = .env$variable,
          stat = list(NULL),
          warning = list(.env$stat[["warning"]]),
          error = list(.env$stat[["error"]])
        )
    }
  }

  df_stat |>
    dplyr::mutate(stat_name = .env$stat_name)
}

