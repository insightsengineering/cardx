#' ARD ANOVA
#'
#' Prepare ANOVA results from the `stats::anova()` function.
#' Users may pass a pre-calculated `stats::anova()` object or a list of
#' formulas. In the latter case, the models will be constructed using the
#' information passed and models will be passed to `stats::anova()`.
#'
#' @param x (`anova` or `data.frame`)\cr
#'   an object of class `'anova'` created with `stats::anova()` or
#'   a data frame
#' @param formulas (`list`)\cr
#'   a list of formulas
#' @param fn (`string`)\cr
#'   string naming the function to be called, e.g. `"glm"`.
#'   If function belongs to a library that is not attached, the package name
#'   must be specified in the `package` argument.
#' @param fn.args (named `list`)\cr
#'   named list of arguments that will be passed to `fn`.
#' @param package (`string`)\cr
#'   string of package name that will be temporarily loaded when function
#'   specified in `method` is executed.
#' @param method (`string`)\cr
#'   string of the method used. Default is `"ANOVA results from `stats::anova()`"`.
#'   We provide the option to change this as `stats::anova()` can produce
#'   results from many types of models that may warrant a more precise
#'   description.
#' @inheritParams rlang::args_dots_empty
#'
#' @details
#' When a list of formulas is supplied to `ard_stats_anova()`, these formulas
#' along with information from other arguments, are used to construct models
#' and pass those models to `stats::anova()`.
#'
#' The models are constructed using `rlang::exec()`, which is similar to `do.call()`.
#'
#' ```r
#' rlang::exec(.fn = fn, formula = formula, data = data, !!!fn.args)
#' ```
#'
#' The above function is executed in `withr::with_namespace(package)`, which
#' allows for the use of `ard_stats_anova(fn)` from packages,
#' e.g. `package = 'lme4'` must be specified when `fn = 'glmer'`.
#' See example below.
#'
#' @return ARD data frame
#' @name ard_stats_anova
#'
#' @examplesIf cards::is_pkg_installed(c("broom", "withr", "lme4"), reference_pkg = "cardx")
#' anova(
#'   lm(mpg ~ am, mtcars),
#'   lm(mpg ~ am + hp, mtcars)
#' ) |>
#'   ard_stats_anova()
#'
#' ard_stats_anova(
#'   x = mtcars,
#'   formulas = list(am ~ mpg, am ~ mpg + hp),
#'   fn = "glm",
#'   fn.args = list(family = binomial)
#' )
#'
#' ard_stats_anova(
#'   x = mtcars,
#'   formulas = list(am ~ 1 + (1 | vs), am ~ mpg + (1 | vs)),
#'   fn = "glmer",
#'   fn.args = list(family = binomial),
#'   package = "lme4"
#' )
NULL

#' @rdname ard_stats_anova
#' @export
ard_stats_anova <- function(x, ...) {
  UseMethod("ard_stats_anova")
}

#' @rdname ard_stats_anova
#' @export
ard_stats_anova.anova <- function(x, method = "ANOVA results from `stats::anova()`", ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_dots_empty()
  check_pkg_installed("broom", reference_pkg = "cardx")
  check_string(method, message = "Argument {.arg method} must be a string of a function name.")

  # return df in cards formats -------------------------------------------------
  lst_results <-
    cards::eval_capture_conditions(
      .anova_tidy_and_reshape(x, method = method)
    )

  # final tidying up of cards data frame ---------------------------------------
  .anova_final_ard_prep(lst_results, method = method)
}


#' @rdname ard_stats_anova
#' @export
ard_stats_anova.data.frame <- function(x,
                                       formulas,
                                       fn,
                                       fn.args = list(),
                                       package = "base",
                                       method = "ANOVA results from `stats::anova()`",
                                       ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_dots_empty()
  check_string(package)
  check_pkg_installed(c("broom", "withr", package), reference_pkg = "cardx")
  check_not_missing(formulas)
  check_not_missing(x)
  check_not_missing(fn)
  check_string(method, message = "Argument {.arg method} must be a string of a function name.")
  check_data_frame(x)
  check_string(fn)
  if (str_detect(fn, "::")) {
    cli::cli_abort(
      c(
        "Argument {.arg fn} cannot be namespaced.",
        i = "Put the package name in the {.arg package} argument."
      ),
      call = get_cli_abort_call()
    )
  }

  # calculate results and return df in cards formats ---------------------------
  # process fn.args argument
  fn.args <- rlang::call_args(rlang::enexpr(fn.args))

  # create models
  lst_results <-
    cards::eval_capture_conditions({
      # first build the models
      models <-
        lapply(
          formulas,
          function(formula) {
            withr::with_namespace(
              package = package,
              call2(.fn = fn, formula = formula, data = x, !!!fn.args) |>
                eval_tidy()
            )
          }
        )

      # now calculate `stats::anova()` and reshape results
      rlang::inject(stats::anova(!!!models)) |>
        .anova_tidy_and_reshape(method = method)
    })

  # final tidying up of cards data frame ---------------------------------------
  .anova_final_ard_prep(lst_results, method = method)
}

.anova_tidy_and_reshape <- function(x, method) {
  broom::tidy(x) |>
    dplyr::mutate(
      across(everything(), as.list),
      variable = paste0("model_", dplyr::row_number())
    ) |>
    tidyr::pivot_longer(
      cols = -"variable",
      names_to = "stat_name",
      values_to = "stat"
    ) |>
    dplyr::filter(!is.na(.data$stat)) %>%
    # add one more row with the method
    {
      dplyr::bind_rows(
        .,
        dplyr::filter(., dplyr::n() == dplyr::row_number()) |>
          dplyr::mutate(
            stat_name = "method",
            stat = list(.env$method)
          )
      )
    }
}

.anova_final_ard_prep <- function(lst_results, method) {
  # saving the results in data frame -------------------------------------------
  df_card <-
    if (!is.null(lst_results[["result"]])) {
      lst_results[["result"]]
    } else { # if there was an error return a shell of an ARD data frame
      dplyr::tibble(
        variable = "model_1",
        stat_name = c("p.value", "method"),
        stat = list(NULL, method)
      )
    }

  # final tidying up of cards data frame ---------------------------------------
  df_card |>
    dplyr::mutate(
      warning = lst_results["warning"],
      error = lst_results["error"],
      context = "stats_anova",
      fmt_fn = lapply(
        .data$stat,
        function(x) {
          switch(is.integer(x),
            0L
          ) %||% switch(is.numeric(x),
            1L
          )
        }
      ),
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "sumsq" ~ "Sum of Squares",
          .data$stat_name %in% "rss" ~ "Residual Sum of Squares",
          .data$stat_name %in% "df" ~ "Degrees of Freedom",
          .data$stat_name %in% "df.residual" ~ "df for residuals",
          .default = .data$stat_name
        )
    ) |>
    cards::tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))} # styler: off
}
