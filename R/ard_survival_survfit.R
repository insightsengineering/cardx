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
#' @param type (`string` or `NULL`)\cr
#'   type of statistic to report. Available for Kaplan-Meier time estimates only, otherwise `type`
#'   is ignored. Default is `NULL`.
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
#' @name ard_survival_survfit
#'
#' @details
#' * Only one of either the `times` or `probs` parameters can be specified.
#' * Times should be provided using the same scale as the time variable used to fit the provided
#'   survival fit model.
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survival", "broom"), reference_pkg = "cardx"))
#' library(survival)
#'
#' survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   ard_survival_survfit(times = c(60, 180))
#'
#' survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   ard_survival_survfit(probs = c(0.25, 0.5, 0.75))
#'
#' # Competing Risks Example ---------------------------
#' set.seed(1)
#' ADTTE_MS <- cards::ADTTE %>%
#'   dplyr::mutate(
#'     CNSR = dplyr::case_when(
#'       CNSR == 0 ~ "censor",
#'       runif(dplyr::n()) < 0.5 ~ "death from cancer",
#'       TRUE ~ "death other causes"
#'     ) %>% factor()
#'   )
#'
#' survfit(Surv(AVAL, CNSR) ~ TRTA, data = ADTTE_MS) %>%
#'   ard_survival_survfit(times = c(60, 180))
NULL

#' @rdname ard_survival_survfit
#' @export
ard_survival_survfit <- function(x, times = NULL, probs = NULL, type = NULL) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed(c("survival", "broom"), reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(x)
  check_class(x, cls = "survfit")
  if (inherits(x, "survfitcox")) {
    cli::cli_abort("Argument {.arg x} cannot be class {.cls survfitcox}.",
      call = get_cli_abort_call()
    )
  }

  # competing risks models cannot use the type argument
  if (inherits(x, c("survfitms", "survfitcoxms")) && !is.null(type)) {
    cli::cli_abort("Cannot use {.arg type} argument with {.code survfit} models with class {.cls {c('survfitms', 'survfitcoxms')}}.",
      call = get_cli_abort_call()
    )
  }
  if (!is.null(probs)) check_range(probs, c(0, 1))
  if (sum(is.null(times), is.null(probs)) != 1) {
    cli::cli_abort("One and only one of {.arg times} and {.arg probs} must be specified.")
  }

  # for regular KM estimators, we allow the type argument
  if (!inherits(x, "survfitms") && !is.null(type)) {
    type <- arg_match(type, values = c("survival", "risk", "cumhaz"))
  }

  # cannot specify type arg when probs supplied
  if (!is.null(probs) && !is.null(type)) {
    cli::cli_abort("Cannot use {.arg type} argument when {.arg probs} argument specifed.",
      call = get_cli_abort_call()
    )
  }

  # build ARD ------------------------------------------------------------------
  est_type <- ifelse(is.null(probs), "times", "probs")
  tidy_survfit <- switch(est_type,
    "times" = .process_survfit_time(x, times, type %||% "survival"),
    "probs" = .process_survfit_probs(x, probs)
  )

  .format_survfit_results(tidy_survfit)
}

#' Process Survival Fit For Time Estimates
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams ard_survival_survfit
#'
#' @return a `tibble`
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survival", "broom"), reference_pkg = "cardx"))
#' survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
#'   cardx:::.process_survfit_time(times = c(60, 180), type = "risk")
#'
#' @keywords internal
.process_survfit_time <- function(x, times, type) {
  # tidy survfit results
  tidy_x <- broom::tidy(x)

  # process competing risks/multi-state models
  multi_state <- inherits(x, "survfitms")

  if (multi_state == TRUE) {
    # selecting state to show
    state <- setdiff(unique(tidy_x$state), "(s0)")[[1]]
    cli::cli_inform("Multi-state model detected. Showing probabilities into state '{state}'.")
    tidy_x <- dplyr::filter(tidy_x, .data$state == .env$state)
  }

  # adding time 0 to data frame
  tidy_x <- tidy_x %>%
    # make strata a fct to preserve ordering
    dplyr::mutate(dplyr::across(dplyr::any_of("strata"), ~ factor(., levels = unique(.)))) %>%
    # if CI is missing and SE is 0, use estimate as the CI
    dplyr::mutate_at(
      dplyr::vars("conf.high", "conf.low"),
      ~ ifelse(is.na(.) & .data$std.error == 0, .data$estimate, .)
    ) %>%
    dplyr::select(dplyr::any_of(c("time", "estimate", "conf.high", "conf.low", "strata"))) %>%
    # add data for time 0
    dplyr::bind_rows(
      dplyr::group_by_at(., dplyr::vars(dplyr::any_of("strata"))) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(
          time = 0,
          estimate = ifelse(multi_state, 0, 1),
          conf.low = ifelse(multi_state, 0, 1),
          conf.high = ifelse(multi_state, 0, 1)
        )
    ) %>%
    dplyr::ungroup()

  strat <- "strata" %in% names(tidy_x)

  # get requested estimates
  df_stat <- tidy_x %>%
    # find max time
    dplyr::group_by_at(., dplyr::vars(dplyr::any_of("strata"))) %>%
    dplyr::mutate(time_max = max(.data$time)) %>%
    dplyr::ungroup() %>%
    # add requested timepoints
    dplyr::full_join(
      tidy_x %>%
        dplyr::select(any_of("strata")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          time = list(.env$times),
          col_name = list(paste("stat", seq_len(length(.env$times)), sep = "_"))
        ) %>%
        tidyr::unnest(cols = c("time", "col_name")),
      by = unlist(intersect(c("strata", "time"), names(tidy_x)))
    )

  if (strat) {
    df_stat <- df_stat %>% dplyr::arrange(.data$strata)
  }

  df_stat <- df_stat %>%
    # if user-specifed time is unobserved, fill estimate with previous value
    dplyr::arrange(.data$time) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::any_of("strata"))) %>%
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

  df_stat <- extract_multi_strata(x, df_stat)

  df_stat
}

#' Process Survival Fit For Quantile Estimates
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams ard_survival_survfit
#'
#' @return a `tibble`
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "survival", reference_pkg = "cardx"))
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
      dplyr::select(dplyr::any_of(c("strata", "estimate", "conf.low", "conf.high"))) %>%
      dplyr::mutate(prob = .x)
  ) %>%
    dplyr::bind_rows() %>%
    `rownames<-`(NULL) %>%
    dplyr::mutate(context = "survival_survfit") %>%
    dplyr::as_tibble()

  if (length(x$n) == 1) df_stat <- df_stat %>% dplyr::select(-"strata")

  df_stat <- extract_multi_strata(x, df_stat)

  df_stat
}

# process multiple stratifying variables
extract_multi_strata <- function(x, df_stat) {
  x_terms <- attr(stats::terms(stats::as.formula(x$call$formula)), "term.labels")
  x_terms <- gsub(".*\\(", "", gsub("\\)", "", x_terms))
  if (length(x_terms) > 1) {
    strata_lvls <- data.frame()

    for (i in df_stat[["strata"]]) {
      i <- gsub(".*\\(", "", gsub("\\)", "", i))
      terms_str <- strsplit(i, paste(c(paste0(x_terms, "="), paste0(", ", x_terms, "=")), collapse = "|"))[[1]]
      s_lvl <- terms_str[nchar(terms_str) > 0]
      strata_lvls <- rbind(strata_lvls, s_lvl)
    }
    if (nrow(strata_lvls) > 0) {
      strata_lvls <- cbind(strata_lvls, t(x_terms))
      names(strata_lvls) <- c(
        t(sapply(seq_along(x_terms), function(i) c(paste0("group", i, "_level"), paste0("group", i))))
      )
      df_stat <- cbind(df_stat, strata_lvls) %>%
        dplyr::select(-"strata")
    }
  }
  df_stat
}

#' Convert Tidied Survival Fit to ARD
#'
#' @inheritParams cards::tidy_as_ard
#'
#' @return an ARD data frame of class 'card'
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survival", "broom"), reference_pkg = "cardx"))
#' cardx:::.format_survfit_results(
#'   broom::tidy(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE))
#' )
#'
#' @keywords internal
.format_survfit_results <- function(tidy_survfit) {
  est <- if ("time" %in% names(tidy_survfit)) "time" else "prob"

  ret <- tidy_survfit %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("estimate", "conf.high", "conf.low", "time", "prob")), ~ as.list(.)
    )) %>%
    tidyr::pivot_longer(
      cols = dplyr::any_of(c("estimate", "conf.high", "conf.low")),
      names_to = "stat_name",
      values_to = "stat"
    ) %>%
    dplyr::mutate(
      variable = est,
      variable_level = .data[[est]]
    ) %>%
    dplyr::select(-all_of(est))

  if ("strata" %in% names(ret)) {
    ret <- ret %>%
      tidyr::separate_wider_delim("strata", "=", names = c("group1", "group1_level"))
  }

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
    dplyr::mutate(dplyr::across(matches("group[0-9]*_level"), ~ as.list(as.factor(.x)))) %>%
    dplyr::mutate(
      warning = list(NULL),
      error = list(NULL)
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
