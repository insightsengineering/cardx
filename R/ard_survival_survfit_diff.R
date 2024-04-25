#' ARD Survival Differences
#'
#' Calculate differences in the Kaplan-Meier estimator of survival using the
#' results from [`survival::survfit()`].
#'
#' @param x (`survift`)\cr
#'   object of class `'survfit'` typically created with [`survival::survfit()`]
#' @inheritParams ard_survival_survfit
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' library(survival)
#'
#' survfit(Surv(time, status) ~ ph.ecog, data = lung) |>
#'   ard_survival_survfit_diff(times = c(100, 200))
ard_survival_survfit_diff <- function(x, times, conf.level = 0.95) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed(c("survival", "broom"), reference_pkg = "cardx")
  check_not_missing(x)
  check_not_missing(times)
  check_class(x, "survfit")
  if (inherits(x, "survfitms")) {
    cli::cli_abort(
      "Argument {.arg x} cannot be class {.cls survfitms}.",
      call = get_cli_abort_call()
    )
  }
  check_range(conf.level, range = c(0, 1))
  check_length(
    as.list(sf$call)[["formula"]] |> as.formula() |> stats::terms() |> attr("term.labels"),
    length = 1L,
    message = "The {.cls survift} object passed in argument {.arg x} must be stratified by a single variable."
  )
  if (length(x$strata) < 2) {
    cli::cli_abort(
      "The {.cls survift} object passed in argument {.arg x} must have more than 1 stratifying level.",
      call = get_cli_abort_call()
    )
  }

  # calculate survival at the specified times
  summary(x, times = times) |>
    tidy_summary.survfit() |>
    dplyr::select(any_of(c("strata", "time", "estimate", "std.error"))) %>%
    {dplyr::left_join(
      dplyr::filter(., .data$strata != .data$strata[1]) |> dplyr::mutate(reference = .$strata[1]),
      dplyr::filter(., .data$strata == .data$strata[1]) |>
        dplyr::select(-"strata") |>
        dplyr::rename_with(.fn = ~paste0(., "0"), .cols = -"time"),
      by = "time"
    )} |>
    dplyr::mutate(
      difference = .data$estimate0 - .data$estimate,
      difference.std.error = sqrt(.data$std.error0^2 + .data$std.error^2),
      statistic = difference / difference.std.error,
      conf.low = difference - difference.std.error * stats::qnorm(1 - (1 - .env$conf.level) / 2),
      conf.high = difference + difference.std.error * stats::qnorm(1 - (1 - .env$conf.level) / 2),
      p.value = 2 * (1 - stats::pnorm(abs(statistic)))
    ) |>
    dplyr::select(
      "strata", "reference", "time",
      estimate = "difference",
      std.error = "difference.std.error",
      "statistic", "conf.low", "conf.high", "p.value"
    ) |>
    tidyr::separate_wider_delim("strata", "=", names = c("group1", "group1_level")) |>
    dplyr::mutate(
      across(-cards::all_ard_groups("names"), as.list)
    ) |>
    tidyr::pivot_longer(
      cols = -c(cards::all_ard_groups(), "time"),
      names_to = "stat_name",
      values_to = "stat"
    ) |>
    dplyr::rename(variable_level = "time") |>
    dplyr::mutate(
      variable = "time",
      error = list(NULL),
      warning = list(NULL),
      fmt_fn = list(1L),
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "reference" ~ "Reference Group (ref - est)",
          .data$stat_name %in% "estimate" ~ "Survival Difference",
          .data$stat_name %in% "std.error" ~ "Survival Difference Standard Error",
          .data$stat_name %in% "conf.low" ~ "CI Lower Bound",
          .data$stat_name %in% "conf.high" ~ "CI Upper Bound",
          .data$stat_name %in% "p.value" ~ "p-value",
          .default = .data$stat_name
        ),
      context = "survival_survfit_diff",
    ) |>
    cards::tidy_ard_column_order() %>%
    structure(., class = c("card", class(.)))
}


tidy_summary.survfit <- function(x) {
  dplyr::tibble(
    strata = x$strata,
    time = x$time,
    n.risk = x$n.risk,
    n.event = x$n.event,
    n.censor = x$n.censor,
    estimate = x$surv,
    std.error = x$std.err,
    conf.low = x$lower,
    conf.high = x$upper
  )
}

