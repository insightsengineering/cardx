#' ARD Incidence Rate
#'
#' @description
#'
#' Function takes a time at risk variable (`time`) and event count variable (`count`) and calculates the incidence
#' rate in person-years.
#'
#' Incidence rate is calculated as: Total number of events that occurred / Total person-time at risk
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param time ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of time at risk variable.
#' @param count ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of variable indicating count of events that occurred. If `NULL`, each row in `data` is assumed to
#'   correspond to a single event occurrence.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name used to identify unique patients in `data`. If `NULL`, each row in `data` is assumed to correspond to
#'   a unique patient.
#' @param conf.level (`numeric`)\cr
#'   confidence level for the estimated incidence rate.
#' @param conf.type (`string`)\cr
#'   confidence interval type for the estimated incidence rate.
#'
#'   One of: `normal` (default), `normal-log`, `exact`, or `byar`.
#' @param units (`string`)\cr
#'   unit of values in `time` and estimated person-time output (e.g. person-years, person-days, etc.). If the desired
#'   person-time estimate unit does not match the current `time` unit, values of `time` should be converted to the
#'   correct unit during pre-processing.
#'
#'   One of: `years`, `months`, `weeks`, or `days`
#' @param n_person_time (`numeric`)\cr
#'   amount of person-time (in unit supplied to `units`) to estimate incidence rate for. Defaults to 100.
#' @inheritParams cards::ard_continuous
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @details
#' The formulas used to calculate the confidence interval for each CI type are as follows, where `x` represents the
#' total number of events that occurred:
#'
#' * `byar`: Byar's approximation of a Poisson CI. A continuity correction of 0.5 is included in the calculation.
#'
#'   `CI lower limit = (x + 0.5) * (1 - 1 / (9 * (x + 0.5)) - stats::qnorm(1 - alpha / 2) / (3 * sqrt(x + 0.5)))^3 / tot_person_time`
#'   `CI upper limit = (x + 0.5) * (1 - 1 / (9 * (x + 0.5)) + stats::qnorm(1 - alpha / 2) / (3 * sqrt(x + 0.5)))^3 / tot_person_time`
#'
#' * `normal`: Normal CI.
#'
#'   `CI lower limit = x / tot_person_time - stats::qnorm(1 - alpha / 2) * sqrt(x) / tot_person_time`
#'   `CI upper limit = x / tot_person_time + stats::qnorm(1 - alpha / 2) * sqrt(x) / tot_person_time`
#'
#' * `normal-log`: Normal-Log CI.
#'
#'   `CI lower limit = exp(log(x / tot_person_time) - stats::qnorm(1 - alpha / 2) / sqrt(x))`
#'   `CI upper limit = exp(log(x / tot_person_time) + stats::qnorm(1 - alpha / 2) / sqrt(x))`
#'
#' * `exact`: Exact CI for a Poisson mean.
#'
#'   `CI lower limit = stats::qchisq(p = alpha / 2, df = 2 * x) / (2 * tot_person_time)`
#'   `CI upper limit = stats::qchisq(p = 1 - alpha / 2, df = 2 * x + 2) / (2 * tot_person_time)`
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(
#'   USUBJID = 1:100,
#'   TRTA = sample(LETTERS[1:3], 100, replace = TRUE),
#'   AETTE1 = abs(rnorm(100, mean = 0.5)),
#'   AETOT1 = sample(0:20, 100, replace = TRUE)
#' )
#'
#' data |>
#'   ard_incidence_rate(time = AETTE1, units = "years", count = AETOT1, id = USUBJID, by = TRTA)
ard_incidence_rate <- function(data,
                               time,
                               units,
                               count = NULL,
                               id = NULL,
                               by = NULL,
                               strata = NULL,
                               conf.level = 0.95,
                               conf.type = c("normal", "normal-log", "exact", "byar"),
                               n_person_time = 100) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_data_frame(data)
  cards::process_selectors(
    data,
    time = {{ time }}, by = {{ by }}, strata = {{ strata }}, count = {{ count }}, id = {{ id }}
  )
  check_length(time, 1)
  check_length(units, 1)
  check_length(count, 1, allow_empty = TRUE)
  check_length(id, 1, allow_empty = TRUE)
  check_class(data[[time]], c("numeric", "integer"))
  check_scalar_range(conf.level, c(0, 1))
  check_numeric(n_person_time)
  check_length(n_person_time, 1)

  conf.type <- arg_match(conf.type, error_call = get_cli_abort_call())
  units <- arg_match(units, values = c("years", "months", "weeks", "days"), error_call = get_cli_abort_call())

  # build ARD ------------------------------------------------------------------
  cards::ard_complex(
    data = data,
    variables = all_of(time),
    by = any_of(by),
    strata = any_of(strata),
    statistic = all_of(time) ~ list(
      incidence_rate =
        .calc_incidence_rate(data, time, units, count, id, by, strata, conf.level, conf.type, n_person_time)
    )
  ) |>
    dplyr::select(-"stat_label") |>
    dplyr::left_join(
      .df_incidence_rate_stat_labels(n_person_time, units),
      by = "stat_name"
    ) |>
    dplyr::mutate(
      stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name),
      context = "incidence_rate",
    ) |>
    cards::as_card() |>
    cards::tidy_ard_column_order() |>
    cards::tidy_ard_row_order()
}

# function to perform calculations -------------------------------------------
.calc_incidence_rate <- function(data, time, units, count, id, by, strata, conf.level, conf.type, n_person_time) {
  cards::as_cards_fn(
    \(x, data, ...) {
      # calculate number of unique IDs with >=1 AE
      n_unique_id <- if (!is_empty(id) && !is_empty(count)) {
        sum(!is.na(unique(data[[id]][data[[count]] > 0])))
      } else if (!is_empty(id)) {
        sum(!is.na(unique(data[[id]])))
      } else {
        nrow(data)
      }

      # calculate total person-years
      tot_person_time <- sum(x, na.rm = TRUE)

      # calculate total number of events
      n_events <- if (!is_empty(count)) sum(data[[count]], na.rm = TRUE) else nrow(data)

      rate_est <- n_events / tot_person_time
      rate_se <- sqrt(rate_est / tot_person_time)
      alpha <- 1 - conf.level
      if (conf.type %in% c("normal", "normal-log")) {
        rate_ci <- if (conf.type == "normal") {
          rate_est + c(-1, 1) * stats::qnorm(1 - alpha / 2) * rate_se
        } else {
          exp(log(rate_est) + c(-1, 1) * stats::qnorm(1 - alpha / 2) * rate_se / rate_est)
        }
        conf.low <- rate_ci[1]
        conf.high <- rate_ci[2]
      } else if (conf.type == "exact") {
        conf.low <- stats::qchisq(p = alpha / 2, df = 2 * n_events) / (2 * tot_person_time)
        conf.high <- stats::qchisq(p = 1 - alpha / 2, df = 2 * n_events + 2) / (2 * tot_person_time)
      } else if (conf.type == "byar") {
        seg_1 <- n_events + 0.5
        seg_2 <- 1 - 1 / (9 * (n_events + 0.5))
        seg_3 <- stats::qnorm(1 - alpha / 2) * sqrt(1 / (n_events + 0.5)) / 3
        conf.low <- seg_1 * ((seg_2 - seg_3)^3) / tot_person_time
        conf.high <- seg_1 * ((seg_2 + seg_3)^3) / tot_person_time
      }

      dplyr::tibble(
        estimate = rate_est * n_person_time,
        std.error = rate_se,
        conf.low = conf.low * n_person_time,
        conf.high = conf.high * n_person_time,
        conf.type = conf.type,
        conf.level = conf.level,
        tot_person_time = tot_person_time,
        n_events = n_events,
        n_unique_id = n_unique_id
      )
    },
    stat_names = c(
      "estimate", "std.error", "conf.low", "conf.high", "conf.type", "conf.level",
      "tot_person_time", "n_events", "n_unique_id"
    )
  )
}

.df_incidence_rate_stat_labels <- function(n_person_time, units) {
  time_unit <- paste0("Person-", str_replace(units, "([[:alpha:]])", substr(toupper(units), 1, 1)))

  dplyr::tribble(
    ~stat_name, ~stat_label,
    "estimate", paste("AE Rate per", n_person_time, time_unit),
    "std.error", "Standard Error",
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "conf.type", "CI Type",
    "conf.level", "CI Confidence Level",
    "tot_person_time", paste(time_unit, "at Risk"),
    "n_events", "Number of AEs Observed",
    "n_unique_id", "Number of Patients with Any AE"
  )
}
