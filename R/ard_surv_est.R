#' ARD Survival Estimates
#'
#' @description
#' Analysis results data for survival quantiles and x-year survival estimates, extracted
#' from a [survival::survfit()] model.
#'
#' @param x ([survival::survfit()])\cr
#'   a [survival::survfit()] object. See below for details.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of the subject or participant ID
#'
#' @return an ARD data frame of class 'card'
#' @name ard_surv_est
#'
#' @details
#' For the `ard_surv_est()` function, the data is expected to be one row per subject.
#' The data is passed as `t.test(data[[variable]] ~ data[[by]], paired = FALSE, ...)`.
#'
#' For the `ard_paired_ttest()` function, the data is expected to be one row
#' per subject per by level. Before the t-test is calculated, the data are
#' reshaped to a wide format to be one row per subject.
#' The data are then passed as
#' `t.test(x = data_wide[[<by level 1>]], y = data_wide[[<by level 2>]], paired = TRUE, ...)`.
#'
#' @examplesIf broom.helpers::.assert_package("survival", pkg_search = "cardx", boolean = TRUE)
#' library(survival)
#'
#' survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   ard_surv_est(times = c(60, 180))
#'
#' survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   ard_surv_est(probs = c(0.25, 0.5, 0.75))
NULL

#' @rdname ard_surv_est
#' @export
ard_surv_est <- function(x, times = NULL, probs = NULL, reverse = FALSE) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("survival", reference_pkg = "cardx")

  # input checks ---------------------------------------------------------------
  if (!all(inherits(x, "survfit"))) {
    stop("Argument `x=` must be class 'survfit' created from the `survival::survfit()` function.",
         call. = FALSE
    )
  }
  if (c(is.null(times), is.null(probs)) %>% sum() != 1) {
    stop("One and only one of `times=` and `probs=` must be specified.", call. = FALSE)
  }
  if (reverse == TRUE && !is.null(probs)) {
    rlang::inform("`reverse=TRUE` argument ignored for survival quantile estimation.")
  }

  # check/process inputs -------------------------------------------------------
  check_not_missing(x)

  # build ARD ------------------------------------------------------------------
  est_type <- ifelse(is.null(probs), "times", "probs")
  tidy_stats <- switch(
    est_type,
    "times" = .format_survfit_time(x, times, reverse),
    "probs" = .format_survfit_probs(x, probs)
  )

  .format_surv_est_results(tidy_stats)
}

.format_survfit_time <- function(x, times, reverse) {
  tidy <- broom::tidy(x)

  strata <- intersect("strata", names(tidy)) %>%
    list() %>%
    compact()
  multi_state <- inherits(x, "survfitms")
  if (multi_state == TRUE) {
    # selecting state to show
    state <- unique(tidy$state) %>%
      setdiff("(s0)") %>%
      purrr::pluck(1)

    tidy <- dplyr::filter(tidy, .data$state == .env$state)
  }

  # adding time 0 to data frame
  tidy <-
    tidy %>%
    # making strata a fct to preserve ordering
    mutate_at(vars(strata), ~ factor(., levels = unique(.))) %>%
    # if CI is missing, and SE is 0, making the CIs the estimate
    mutate_at(
      vars("conf.high", "conf.low"),
      ~ ifelse(is.na(.) & .data$std.error == 0, .data$estimate, .)
    ) %>%
    select(any_of(c("time", "estimate", "conf.high", "conf.low", "strata"))) %>%
    bind_rows(
      group_by(., strata) %>%
        slice(1) %>%
        mutate(
          time = 0,
          estimate = 1,
          conf.low = 1,
          conf.high = 1
        )
    ) %>%
    ungroup()

  # getting requested estimates
  df_stat <-
    tidy %>%
    # getting the latest time (not showing estimates after that time)
    group_by(., strata) %>%
    mutate(time_max = max(.data$time)) %>%
    ungroup() %>%
    # adding in timepoints requested by user
    full_join(
      select(tidy, strata) %>%
        distinct() %>%
        mutate(
          time = list(.env$times),
          col_name = list(paste("stat", seq_len(length(.env$times)), sep = "_"))
        ) %>%
        unnest(cols = c("time", "col_name")),
      by = unlist(c(strata, "time"))
    ) %>%
    # if the user-specifed time is unobserved, filling estimates with previous value
    arrange(strata, .data$time) %>%
    group_by(strata) %>%
    tidyr::fill(
      "estimate", "conf.high", "conf.low", "time_max",
      .direction = "down"
    ) %>%
    ungroup() %>%
    # keeping obs of user-specified times
    filter(!is.na(.data$col_name)) %>%
    # if user-specified time is after the latest follow-up time, making it NA
    mutate_at(
      vars("estimate", "conf.high", "conf.low"),
      ~ ifelse(.data$time > .data$time_max, NA_real_, .)
    ) %>%
    select(-c(time_max, col_name))

  # converting to reverse probs, if requested
  if (reverse == TRUE) {
    df_stat <-
      df_stat %>%
      mutate_at(vars("estimate", "conf.low", "conf.high"), ~ 1 - .) %>%
      dplyr::rename(conf.low = "conf.high", conf.high = "conf.low")
  }

  df_stat
}

.format_survfit_probs <- function(x, probs) {
  tidy <- broom::tidy(x)

  strata <- intersect("strata", names(tidy)) %>%
    list() %>%
    compact()

  # calculating survival quantiles, and adding estimates to pretty tbl
  df_stat <- purrr::map2_dfr(
    probs, seq_along(probs),
    ~ stats::quantile(x, probs = .x) %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      set_names(c("strata", "estimate", "conf.low", "conf.high")) %>%
      mutate(
        prob = .x
      )
  )

  df_stat
}

#' Convert Tidied Survival Fit to ARD
#'
#' @inheritParams cards::tidy_as_ard
#'
#' @return an ARD data frame of class 'card'
#' @keywords internal
#'
#' @examples
#' cardx:::.format_surv_est_results(
#'   broom::tidy(survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE))
#' )
.format_surv_est_results <- function(tidy_stats) {
  ret <- tidy_stats %>%
    mutate(across(
      any_of(c("estimate", "conf.high", "conf.low", "time", "prob")), ~ as.list(.)
    )) %>%
    pivot_longer(
      cols = any_of(c("estimate", "conf.high", "conf.low", "time", "prob")),
      names_to = "stat_name",
      values_to = "stat"
    ) %>%
    separate_wider_delim(strata, "=", names = c("variable", "variable_level"))

  # summarize model ------------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_survest_stat_labels(),
      by = "stat_name"
    ) |>
    dplyr::mutate(
      fmt_fn =
        lapply(
          .data$stat,
          function(x) {
            switch(is.integer(x), 0L) %||% switch(is.numeric(x), 1L)
          }
        ),
      context = "survival",
  # add the stat label ---------------------------------------------------------
      stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)
    ) |>
    cards::tidy_ard_column_order() %>%
    structure(., class = c("card", class(.)))
}

.df_survest_stat_labels <- function() {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "estimate", "Survival Probability",
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "conf.level", "CI Confidence Level",
    "prob", "Quantile",
    "times", "Time"
  )
}
