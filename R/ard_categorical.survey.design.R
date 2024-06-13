#' ARD Categorical Survey Statistics
#'
#' @description
#' Compute tabulations on survey-weighted data.
#'
#' The counts and proportion (`"N"`, `"n"`, `"p"`) are calculated using `survey::svytable()`,
#' and the standard errors and design effect (`"p.std.error"`, `"deff"`) are
#' calculated using `survey::svymean()`.
#'
#' @param data (`survey.design`)\cr
#'   a design object often created with [`survey::svydesign()`].
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to include in summaries. Default is `everything()`.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   results are calculated for **all combinations** of the column specified
#'   and the variables. A single column may be specified.
#' @param denominator (`string`)\cr
#'   a string indicating the type proportions to calculate. Must be one of
#'   `"column"` (the default), `"row"`, and `"cell"`.
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   a named list, a list of formulas,
#'   or a single formula where the list element is a character vector of
#'   statistic names to include. See default value for options.
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
#' @inheritParams rlang::args_dots_empty
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examplesIf cardx:::is_pkg_installed("survey", reference_pkg = "cardx")
#' svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
#'
#' ard_categorical(svy_titanic, variables = Class, by = Survived)
ard_categorical.survey.design <- function(data,
                                          variables = everything(),
                                          by = NULL,
                                          statistic = everything() ~ c("n", "N", "p", "p.std.error", "deff"),
                                          denominator = c("column", "row", "cell"),
                                          fmt_fn = NULL,
                                          stat_label = everything() ~ list(p = "%", p.std.error = "SE(%)", deff = "Design Effect"),
                                          ...) {
  set_cli_abort_call()
  deff <- TRUE # we may update in the future to make this an argument for users

  # process arguments ----------------------------------------------------------
  cards::process_selectors(
    data = data$variables,
    variables = {{ variables }},
    by = {{ by }}
  )
  variables <- setdiff(variables, by)
  check_scalar(by, allow_empty = TRUE)

  cards::process_formula_selectors(
    data = data$variables[variables],
    statistic = statistic,
    fmt_fn = fmt_fn,
    stat_label = stat_label
  )
  cards::fill_formula_selectors(
    data = data$variables[variables],
    statistic = formals(asNamespace("cardx")[["ard_categorical.survey.design"]])[["statistic"]] |> eval(),
  )
  accepted_svy_stats <- c("n", "N", "p", "p.std.error", "deff")
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) all(x %in% accepted_svy_stats),
    error_msg = c("Error in the values of the {.arg statistic} argument.",
                  i = "Values must be in {.val {accepted_svy_stats}}"
    )
  )
  denominator <- arg_match(denominator)

  # check the missingness
  walk(
    variables,
    \(.x) {
      if (all(is.na(data$variables[[.x]])) && !inherits(.x, c("logical", "factor"))) {
        cli::cli_abort(
          c("Column {.val {.x}} is all missing and cannot be tabulated.",
            i = "Only columns of class {.cls logical} and {.cls factor} can be tabulated when all values are missing.")
        )
      }
    }
  )

  # calculate counts -----------------------------------------------------------
  # this tabulation accounts for unobserved combinations
  svytable_counts <- .svytable_counts(data, variables, by, denominator)

  # calculate rates along with SE and DEFF -------------------------------------
  svytable_rates <- .svytable_rates(data, variables, by, denominator, deff)

  # convert results into a proper ARD object -----------------------------------
  cards <-
    svytable_counts |>
    dplyr::left_join(
      svytable_rates |> dplyr::select(-"p"),
      by = intersect(c("group1", "group1_level", "variable", "variable_level"), names(svytable_counts))
    ) |>
    dplyr::mutate(across(-any_of(c("group1", "variable")), as.list)) |>
    tidyr::pivot_longer(
      cols = -c(cards::all_ard_groups(), cards::all_ard_variables()),
      names_to = "stat_name",
      values_to = "stat"
    ) |>
    # keep statistics requested by user
    dplyr::inner_join(
      statistic |> enframe("variable", "stat_name") |> tidyr::unnest(cols = "stat_name"),
      by = c("variable", "stat_name")
    )

  # final processing of fmt_fn -------------------------------------------------
  cards <- cards |>
    .process_nested_list_as_df(
      arg = fmt_fn,
      new_column = "fmt_fn"
    ) |>
    .default_svy_cat_fmt_fn()

  # merge in statistic labels --------------------------------------------------
  cards <- cards |>
    .process_nested_list_as_df(
      arg = stat_label,
      new_column = "stat_label",
      unlist = TRUE
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name))

  # return final object --------------------------------------------------------
  cards |>
    dplyr::mutate(
      context = "categorical",
      warning = list(NULL),
      error = list(NULL),
    ) |>
    cards::tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))} # styler: off
}

.svytable_rates <- function(data, variables, by, denominator, deff) {
  lapply(
    variables,
    \(variable) {
      case_switch(
        # first chunk with by variable specified
        !is_empty(by) && denominator == "column" ~
          .one_svytable_rates_by_column(data, variable, by, deff),
        !is_empty(by) && denominator == "row" ~
          .one_svytable_rates_by_row(data, variable, by, deff),
        !is_empty(by) && denominator == "cell" ~
          .one_svytable_rates_by_cell(data, variable, by, deff),
        # this chunk without a by variable
        denominator %in% c("column", "cell") ~
          .one_svytable_rates_no_by_column_and_cell(data, variable, deff),
        denominator == "row" ~
          .one_svytable_rates_no_by_row(data, variable, deff)
      )
    }
  ) |>
    dplyr::bind_rows()
}

.one_svytable_rates_no_by_row <- function(data, variable, deff) {
  dplyr::tibble(
    variable = .env$variable,
    variable_level = unique(data$variables[[variable]]) |> sort(),
    p = 1,
    p.std.error = 0,
    deff = NaN
  )
}

.one_svytable_rates_no_by_column_and_cell <- function(data, variable, deff) {
  survey::svymean(reformulate2(variable), design = data, na.rm = TRUE, deff = deff) |>
    dplyr::as_tibble(rownames = "var_level") |>
    dplyr::mutate(
      variable_level = str_remove(.data$var_level, pattern = paste0("^", .env$variable)),
      variable = .env$variable
    ) |>
    dplyr::select("variable", "variable_level", p = "mean", p.std.error = "SE", any_of("deff"))
}

.one_svytable_rates_by_cell <- function(data, variable, by, deff) {
  df_interaction_id <-
    .df_all_combos(data, variable, by) |>
    dplyr::mutate(
      var_level =
        glue::glue("interaction({.env$by}, {.env$variable}){.data$group1_level}.{.data$variable_level}")
    )

  survey::svymean(
    x = inject(~interaction(!!sym(bt(by)), !!sym(bt(variable)))),
    design = data,
    na.rm = TRUE,
    deff = deff
  ) |>
    dplyr::as_tibble(rownames = "var_level") |>
    dplyr::left_join(df_interaction_id, by = "var_level") |>
    dplyr::select(
      cards::all_ard_groups(), cards::all_ard_variables(),
      p = "mean", p.std.error = "SE", any_of("deff")
    )
}

.one_svytable_rates_by_row <- function(data, variable, by, deff) {
  survey::svyby(
    formula = reformulate2(by),
    by = reformulate2(variable),
    design = data,
    FUN = survey::svymean,
    na.rm = TRUE,
    deff = deff
  ) |>
    dplyr::as_tibble() |>
    tidyr::pivot_longer(-all_of(variable)) |>
    dplyr::mutate(
      stat =
        dplyr::case_when(
          startsWith(.data$name, paste0("se.", by)) | startsWith(.data$name, paste0("se.`", by, "`")) ~ "p.std.error",
          startsWith(.data$name, paste0("DEff.", by)) | startsWith(.data$name, paste0("DEff.`", by, "`")) ~ "deff",
          TRUE ~ "p"
        ),
      name =
        str_remove_all(.data$name, "se\\.") %>%
        str_remove_all("DEff\\.") %>%
        str_remove_all(by) %>%
        str_remove_all("`")
    ) |>
    tidyr::pivot_wider(names_from = "stat", values_from = "value") |>
    set_names(c("variable_level", "group1_level", "p", "p.std.error", "deff")) |>
    dplyr::mutate(
      group1 = .env$by,
      variable = .env$variable,
      across(c("group1_level", "variable_level"), as.character)
    )
}

.one_svytable_rates_by_column <- function(data, variable, by, deff) {
  survey::svyby(
    formula = reformulate2(variable),
    by = reformulate2(by),
    design = data,
    FUN = survey::svymean,
    na.rm = TRUE,
    deff = deff
  ) |>
    dplyr::as_tibble() |>
    tidyr::pivot_longer(-all_of(by)) |>
    dplyr::mutate(
      stat =
        dplyr::case_when(
          startsWith(.data$name, paste0("se.", variable)) | startsWith(.data$name, paste0("se.`", variable, "`")) ~ "p.std.error",
          startsWith(.data$name, paste0("DEff.", variable)) | startsWith(.data$name, paste0("DEff.`", variable, "`")) ~ "deff",
          TRUE ~ "p"
        ),
      name =
        str_remove_all(.data$name, "se\\.") %>%
        str_remove_all("DEff\\.") %>%
        str_remove_all(variable) %>%
        str_remove_all("`")
    ) |>
    tidyr::pivot_wider(names_from = "stat", values_from = "value") |>
    set_names(c("group1_level", "variable_level", "p", "p.std.error", "deff")) |>
    dplyr::mutate(
      group1 = .env$by,
      variable = .env$variable,
      across(c("group1_level", "variable_level"), as.character)
    )
}

.svytable_counts <- function(data, variables, by, denominator) {
  df_counts <-
    lapply(
      variables,
      \(variable) {
        # perform weighted tabulation
        df_count <-
          survey::svytable(formula = reformulate2(c(by, variable)), design = data) |>
          dplyr::as_tibble()
        if (is_empty(by)) {
          names(df_count) <- c("variable_level", "n")
          df_count$variable <- variable
        }
        else {
          names(df_count) <- c("group1_level", "variable_level", "n")
          df_count$variable <- variable
          df_count$group1 <- by
        }

        # adding unobserved levels
        .df_all_combos(data, variable, by) %>%
          dplyr::left_join(
            df_count,
            by = names(.)
          ) |>
          tidyr::replace_na(list(n = 0)) # unobserved levels assigned zero count
      }
    ) |>
    dplyr::bind_rows()

  # add big N and p, then return data frame of results
  switch(
    denominator,
    "column" =
      df_counts |>
      dplyr::mutate(
        .by = any_of("group1_level"),
        N = sum(.data$n),
        p = .data$n / .data$N
      ),
    "row" =
      df_counts |>
      dplyr::mutate(
        .by = any_of("variable_level"),
        N = sum(.data$n),
        p = .data$n / .data$N
      ),
    "cell" =
      df_counts |>
      dplyr::mutate(
        .by = any_of(c("group1_level", "variable_level")),
        N = sum(.data$n),
        p = .data$n / .data$N
      )
  )
}

.df_all_combos <- function(data, variable, by) {
  df <- cards::nest_for_ard(
    data = data$variables,
    by = c(by, variable),
    list_columns = FALSE,
    include_data = FALSE
  )

  # renaming with variable colnames
  if (!is_empty(by)) {
    df <- dplyr::rename(df, variable = "group2", variable_level = "group2_level")
  }
  else {
    df <- dplyr::rename(df, variable = "group1", variable_level = "group1_level")
  }

  # convert levels to character for merging later
  df |>
    dplyr::mutate(
      across(
        c(cards::all_ard_groups("levels"), cards::all_ard_variables("levels")),
        as.character
      )
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

.default_svy_cat_fmt_fn <- function(x) {
  x |>
    dplyr::mutate(
      fmt_fn =
        pmap(
          list(.data$stat_name, .data$stat, .data$fmt_fn),
          function(stat_name, stat, fmt_fn) {
            if (!is_empty(fmt_fn)) {
              return(fmt_fn)
            }
            if (stat_name %in% c("p", "p_miss", "p_nonmiss")) {
              return(cards::label_cards(digits = 1, scale = 100))
            }
            if (stat_name %in% c("n", "N", "N_miss", "N_nonmiss", "N_obs")) {
              return(cards::label_cards(digits = 0))
            }
            if (is.integer(stat)) {
              return(0L)
            }
            if (is.numeric(stat)) {
              return(1L)
            }
            return(as.character)
          }
        )
    )
}

#' Convert Nested Lists to Column
#'
#' Some arguments, such as `stat_label`, are passed as nested lists. This
#' function properly unnests these lists and adds them to the results data frame.
#'
#' @param x (`data.frame`)\cr
#'   result data frame
#' @param arg (`list`)\cr
#'   the nested list
#' @param new_column (`string`)\cr
#'   new column name
#' @param unlist (`logical`)\cr
#'   whether to fully unlist final results
#'
#' @return a data frame
#' @keywords internal
#'
#' @examples
#' ard <- ard_categorical(cards::ADSL, by = "ARM", variables = "AGEGR1")
#'
#' cardx:::.process_nested_list_as_df(ard, NULL, "new_col")
.process_nested_list_as_df <- function(x, arg, new_column, unlist = FALSE) {
  # add fmt_fn column if not already present
  if (!new_column %in% names(x)) {
    x[[new_column]] <- list(NULL)
  }

  # process argument if not NULL, and update new column
  if (!is_empty(arg)) {
    df_argument <-
      imap(
        arg,
        function(enlst_arg, variable) {
          lst_stat_names <-
            x[c("variable", "stat_name")] |>
            dplyr::filter(.data$variable %in% .env$variable) |>
            unique() %>%
            {stats::setNames(as.list(.[["stat_name"]]), .[["stat_name"]])} # styler: off

          cards::compute_formula_selector(
            data = lst_stat_names,
            x = enlst_arg
          ) %>%
            # styler: off
            {dplyr::tibble(
              variable = variable,
              stat_name = names(.),
              "{new_column}" := unname(.)
            )}
          # styler: on
        }
      ) |>
      dplyr::bind_rows()

    x <- x |> dplyr::rows_update(df_argument, by = c("variable", "stat_name"), unmatched = "ignore")
  }

  if (isTRUE(unlist)) {
    x[[new_column]] <- lapply(x[[new_column]], function(x) x %||% NA) |> unlist()
  }

  x
}

