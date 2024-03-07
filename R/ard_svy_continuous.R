#' ARD Continuous Survey Statistics
#'
#' Returns an ARD of weighted statistics using the `{survey}` package.
#'
#' @param data (`survey.design`)\cr
#'   a design object often created with [`survey::svydesign()`].
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to include in summaries. Default is `everything()`.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   results are calculated for **all combinations** of the columns specified,
#'   including unobserved combinations and unobserved factor levels.
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   a named list, a list of formulas,
#'   or a single formula where the list element is a character vector of
#'   statistic names to include. See below for options.
#' @param fmt_fn ([`formula-list-selector`][syntax])\cr
#'   a named list, a list of formulas,
#'   or a single formula where the list element is a named list of functions
#'   (or the RHS of a formula),
#'   e.g. `list(mpg = list(mean = \(x) round(x, digits = 2) |> as.character))`.
#' @param stat_label ([`formula-list-selector`][syntax])\cr
#'   a named list, a list of formulas, or a single formula where
#'   the list element is either a named list or a list of formulas defining the
#'   statistic labels, e.g. `everything() ~ list(mean = "Mean", sd = "SD")` or
#'   `everything() ~ list(mean ~ "Mean", sd ~ "SD")`.
#'
#' @section statistic argument:
#'
#' The following statistics are available:
#' `r cardx:::accepted_svy_stats(FALSE) |> shQuote() |> paste(collapse = ", ")`,
#' where 'p##' is are the percentiles and `##` is an integer between 0 and 100.
#'
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examplesIf cards::is_pkg_installed("survey", reference_pkg = "cardx")
#' data(api, package = "survey")
#' dclus1 <- survey::svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
#'
#' ard_svy_continuous(
#'   data = dclus1,
#'   variables = api00,
#'   by = stype
#' )
ard_svy_continuous <- function(data, variables, by = NULL,
                               statistic = everything() ~ c("median", "p25", "p75"),
                               fmt_fn = NULL,
                               stat_label = NULL) {
  cards::check_pkg_installed("survey", reference_pkg = "cardx")

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_class(data, cls = "survey.design")
  check_not_missing(variables)

  # process inputs -------------------------------------------------------------
  cards::process_selectors(data$variables, variables = {{ variables }}, by = {{ by }})
  variables <- setdiff(variables, by)
  cards::process_formula_selectors(
    data$variables[variables],
    statistic = statistic,
    fmt_fn = fmt_fn,
    stat_label = stat_label
  )
  cards::fill_formula_selectors(
    data$variables[variables],
    statistic = formals(ard_svy_continuous)[["statistic"]] |> eval()
  )
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) all(x %in% accepted_svy_stats()),
    error_msg = c("Error in the values of the {.arg statistic} argument.",
                  i = "Values must be in {.val {accepted_svy_stats(FALSE)}}")
  )

  # compute the weighted statistics --------------------------------------------
  df_stats <-
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
    dplyr::bind_rows()


  # add stat_labels ------------------------------------------------------------
  df_stats <-
    df_stats |>
    dplyr::left_join(
      .default_svy_stat_labels(),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name))
  if (!is_empty(stat_label)) {
    df_stats <-
      dplyr::rows_update(
        df_stats,
        dplyr::tibble(
          stat_name = names(stat_label),
          stat_label = unlist(stat_label) |> unname()
        ),
        by = "stat_name",
        unmatched = "ignore"
      )
  }

  # add formatting stats -------------------------------------------------------
  df_stats$fmt_fn <- list(1L)
  if (!is_empty(stat_label)) {
    df_stats <-
      dplyr::rows_update(
        df_stats,
        dplyr::tibble(
          stat_name = names(fmt_fn),
          fmt_fn = unname(fmt_fn)
        ),
        by = "stat_name",
        unmatched = "ignore"
      )
  }

  # add class and return ARD object --------------------------------------------
  df_stats |>
    dplyr::mutate(context = "continuous") |>
    cards::tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))} # styler: off
}

.default_svy_stat_labels <- function(stat_label = NULL) {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "mean", "Mean",
    "median", "Median",
    "var", "Variance",
    "sd", "SD",
    "sum", "Sum",
    "deff", "Design Effect",
    "mean.std.error", "SE(Mean)",
    "min", "Minimum",
    "max", "Maximum",
    "p25", "25% Percentile",
    "p75", "75% Percentile"
  )
}

accepted_svy_stats <- function(expand_quantiles = TRUE) {
  base_stats <-
    c("mean", "median", "min", "max", "sum", "var", "sd", "mean.std.error", "deff")
  if (expand_quantiles){
    return(c(base_stats, paste0("p", 0:100)))
  }
  c(base_stats, "p##")
}



# this function calculates the summary for a single variable, single statistic
# and for all `by` levels. it returns an ARD data frame
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
  else if (stat_name %in% "min") args <- list(FUN = \(x, design, na.rm, ...) min(design$variables[[all.vars(x)]], na.rm = na.rm))
  else if (stat_name %in% "max") args <- list(FUN = \(x, design, na.rm, ...) max(design$variables[[all.vars(x)]], na.rm = na.rm))
  # define functions for the quantiles
  else if (stat_name %in% c("median", paste0("p", 0:100))) {
    quantile <- ifelse(stat_name %in% "median", 0.5, substr(stat_name, 2, nchar(stat_name)) |> as.numeric() %>% `/`(100))
    # univariate results are returned in a different format from stratified.
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
    args$x <- stats::reformulate(variable)
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
    args$formula <- stats::reformulate(variable)
    args$by <- stats::reformulate(by)
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
        dplyr::as_tibble() %>%
        # adding unobserved combinations of "by" variables
        {dplyr::full_join(
          cards::nest_for_ard(data$variables, by = by, key = "...ard_no_one_will_ever_pick_this...", rename = FALSE, list_columns = FALSE) |>
            dplyr::select(-"...ard_no_one_will_ever_pick_this..."),
          .,
          by = by
        )} |>
        set_names(paste0("group", seq_along(by), "_level"), "stat") |>
        dplyr::bind_cols(
          dplyr::tibble(!!!c(by, variable)) |>
            set_names(paste0("group", seq_along(by)), "variable")
        ) |>
        dplyr::mutate(
          dplyr::across(c(cards::all_ard_groups("levels"), "stat"), as.list),
          warning = list(.env$stat[["warning"]]),
          error = list(.env$stat[["error"]])
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

