#' ARD Survival Estimates
#'
#' @description
#' Analysis results data for survival quantiles and x-year survival estimates, extracted
#' from a [survival::survfit()] model.
#'
#' @param x ([survival::survfit()])\cr
#'   a [survival::survfit()] object. See below for details.
#' @param times (`numeric`)\cr
#'   a vector of times for which to return survival probabilities.
#' @param probs (`numeric`)\cr
#'   a vector of probabilities with values in (0,1) specifying the survival quantiles to return.
#' @param type (`character` or `NULL`)\cr
#'   type of statistic to report. Available for Kaplan-Meier time estimates only, otherwise `type`
#'   is ignored. Default is `"survival"`.
#'   Must be one of the following:
#'   ```{r, echo = FALSE}
#'   dplyr::tribble(
#'     ~type,          ~transformation,
#'     '`"survival"`', '`x`',
#'     '`"risk"`',     '`1 - x`',
#'     '`"cumhaz"`',   '`-log(x)`',
#'   ) %>%
#'   knitr::kable()
#'   ```
#'
#' @return an ARD data frame of class 'card'
#' @name ard_survfit
#'
#' @details
#' * Only one of either the `times` or `probs` parameters can be specified.
#' * Times should be provided using the same scale as the time variable used to fit the provided
#'   survival fit model.
#'
#' @examplesIf cards::is_pkg_installed("survival", reference_pkg = "cardx")
#' library(survival)
#'
#' survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   ard_survfit(times = c(60, 180))
#'
#' survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   ard_survfit(probs = c(0.25, 0.5, 0.75))
NULL

#' @rdname ard_survfit
#' @export
ard_survfit <- function(x, times = NULL, probs = NULL, type = "survival") {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("survival", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(x)
  check_class(type, "character")
  if (!is.null(probs)) check_range(probs, c(0, 1))
  if (!all(inherits(x, "survfit"))) {
    cli::cli_abort(
      "The {.arg x} argument must be class {.cls survfit} created using the {.fun survival::survfit} function."
    )
  }
  if (sum(is.null(times), is.null(probs)) != 1) {
    cli::cli_abort("One and only one of {.arg times} and {.arg probs} must be specified.")
  }
  if (!is.null(times) && !is.null(type) && !type %in% c("survival", "risk", "cumhaz")) {
    cli::cli_abort(
      "The {.arg type} argument is {.val {type}} but must be one of {.val survival}, {.val risk}, or {.val cumhaz}."
    )
  }
  if (type != "survival" && !is.null(probs)) {
    cli::cli_inform("The {.arg type} argument is ignored for survival quantile estimation.")
  }

  # build ARD ------------------------------------------------------------------
  est_type <- ifelse(is.null(probs), "times", "probs")
  tidy_survfit <- switch(est_type,
    "times" = .process_survfit_time(x, times, type),
    "probs" = .process_survfit_probs(x, probs)
  )

  .format_survfit_results(tidy_survfit)
}

#' Process Survival Fit For Time Estimates
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams ard_survfit
#'
#' @return a `tibble`
#'
#' @examples
#' survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   cardx:::.process_survfit_time(times = c(60, 180), type = "risk")
#'
#' @keywords internal
.process_survfit_time <- function(x, times, type) {
  # process multi-state models
  multi_state <- inherits(x, "survfitms")
  if (multi_state == TRUE) {
    # selecting state to show
    state <- setdiff(unique(x$state), "(s0)")[[1]]
    cli::cli_inform("Multi-state model detected. Showing probabilities into state '{state}'.")
    x <- dplyr::filter(x, .data$state == .env$state)
  }

  # tidy survfit results
  tidy_x <- broom::tidy(x)

  # adding time 0 to data frame
  tidy_x <- tidy_x %>%
    # make strata a fct to preserve ordering
    dplyr::mutate_at("strata", ~ factor(., levels = unique(.))) %>%
    # if CI is missing and SE is 0, use estimate as the CI
    dplyr::mutate_at(
      dplyr::vars("conf.high", "conf.low"),
      ~ ifelse(is.na(.) & .data$std.error == 0, .data$estimate, .)
    ) %>%
    dplyr::select(dplyr::any_of(c("time", "estimate", "conf.high", "conf.low", "strata"))) %>%
    # add data for time 0
    dplyr::bind_rows(
      dplyr::group_by(., .data$strata) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(
          time = 0,
          estimate = ifelse(multi_state, 0, 1),
          conf.low = ifelse(multi_state, 0, 1),
          conf.high = ifelse(multi_state, 0, 1)
        )
    ) %>%
    dplyr::ungroup()

  # get requested estimates
  df_stat <- tidy_x %>%
    # find max time
    dplyr::group_by(., .data$strata) %>%
    dplyr::mutate(time_max = max(.data$time)) %>%
    dplyr::ungroup() %>%
    # add requested timepoints
    dplyr::full_join(
      tidy_x %>%
        dplyr::select("strata") %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          time = list(.env$times),
          col_name = list(paste("stat", seq_len(length(.env$times)), sep = "_"))
        ) %>%
        tidyr::unnest(cols = c("time", "col_name")),
      by = unlist(c("strata", "time"))
    ) %>%
    # if user-specifed time is unobserved, fill estimate with previous value
    dplyr::arrange(.data$strata, .data$time) %>%
    dplyr::group_by(.data$strata) %>%
    tidyr::fill(
      "estimate", "conf.high", "conf.low", "time_max",
      .direction = "down"
    ) %>%
    dplyr::ungroup() %>%
    # keep only user-specified times
    dplyr::filter(!is.na(.data$col_name)) %>%
    # if user-specified time is after max time, make estimate NA
    dplyr::mutate_at(
      dplyr::vars("estimate", "conf.high", "conf.low"),
      ~ ifelse(.data$time > .data$time_max, NA_real_, .)
    ) %>%
    dplyr::mutate(context = type) %>%
    dplyr::select(!dplyr::any_of(c("time_max", "col_name")))

  # convert estimates to requested type
  if (type != "survival") {
    df_stat <- df_stat %>%
      dplyr::mutate(dplyr::across(
        any_of(c("estimate", "conf.low", "conf.high")),
        if (type == "cumhaz") ~ -log(.x) else ~ 1 - .x
      )) %>%
      dplyr::rename(conf.low = "conf.high", conf.high = "conf.low")
  }

  df_stat
}

#' Process Survival Fit For Quantile Estimates
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams ard_survfit
#'
#' @return a `tibble`
#'
#' @examples
#' survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   cardx:::.process_survfit_probs(probs = c(0.25, 0.75))
#'
#' @keywords internal
.process_survfit_probs <- function(x, probs) {
  # calculate survival quantiles and add estimates to df
  df_stat <- map2(
    probs,
    seq_along(probs),
    ~ stats::quantile(x, probs = .x) %>%
      as.data.frame() %>%
      set_names(c("estimate", "conf.low", "conf.high")) %>%
      dplyr::mutate(strata = row.names(.)) %>%
      dplyr::select(strata, estimate, conf.low, conf.high) %>%
      dplyr::mutate(prob = .x)
  ) %>%
    dplyr::bind_rows() %>%
    `rownames<-`(NULL) %>%
    dplyr::mutate(context = "survival") %>%
    dplyr::as_tibble()

  df_stat
}

#' Convert Tidied Survival Fit to ARD
#'
#' @inheritParams cards::tidy_as_ard
#'
#' @return an ARD data frame of class 'card'
#'
#' @examples
#' cardx:::.format_survfit_results(
#'   broom::tidy(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE))
#' )
#'
#' @keywords internal
.format_survfit_results <- function(tidy_survfit) {
  ret <- tidy_survfit %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("estimate", "conf.high", "conf.low", "time", "prob")), ~ as.list(.)
    )) %>%
    tidyr::pivot_longer(
      cols = dplyr::any_of(c("estimate", "conf.high", "conf.low", "time", "prob")),
      names_to = "stat_name",
      values_to = "stat"
    ) %>%
    tidyr::separate_wider_delim("strata", "=", names = c("variable", "variable_level"))

  ret %>%
    dplyr::left_join(
      .df_survfit_stat_labels(),
      by = "stat_name"
    ) %>%
    dplyr::mutate(
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
      stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)
    ) %>%
    structure(., class = c("card", class(.))) %>%
    cards::tidy_ard_column_order() %>%
    cards::tidy_ard_row_order()
}

.df_survfit_stat_labels <- function() {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "estimate", "Survival Probability",
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "conf.level", "CI Confidence Level",
    "prob", "Quantile",
    "time", "Time"
  )
}
