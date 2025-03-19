#' ARD Incidence Rate
#'
#' Function takes a time at risk variable (`time`) and event count variable (`count`) and calculates the incidence
#' rate in person years.
#'
#' Incidence rate is calculated as:
#'
#' Total number of events that occurred / Total person-time at risk
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
#'   unit of values in `time`.
#'
#'   One of: `years`, `months`, `weeks`, or `days`
#' @param n_person_years (`numeric`)\cr
#'   number of person-years to estimate incidence rate for. Defaults to 100.
#' @inheritParams cards::ard_continuous
#'
#' @return an ARD data frame of class 'card'
#' @export
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
                               n_person_years = 100) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(time)
  check_not_missing(units)
  check_data_frame(data)
  cards::process_selectors(
    data,
    time = {{ time }}, by = {{ by }}, strata = {{ strata }}, count = {{ count }}, id = {{ id }}
  )
  check_class(data[[time]], c("numeric", "integer"))
  check_scalar_range(conf.level, c(0, 1))
  check_numeric(n_person_years)

  conf.type <- arg_match(conf.type, error_call = get_cli_abort_call())
  units <- arg_match(units, values = c("years", "months", "weeks", "days"), error_call = get_cli_abort_call())

  # calculate incidence rate & related statistics ------------------------------
  calc_incidence_rate <- cards::as_cards_fn(
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
      tot_person_years <- sum(x, na.rm = TRUE) *
        dplyr::case_when(
          units == "months" ~ 1 / 12, # months per year
          units == "weeks" ~ 1 / (365.25 / 7), # weeks per year
          units == "days" ~ 1 / 365.25, # days per year
          TRUE ~ 1
        )

      # calculate total number of events
      n_events <- if (!is_empty(count)) sum(data[[count]], na.rm = TRUE) else nrow(data)

      rate_est <- n_events / tot_person_years
      alpha <- 1 - conf.level
      if (conf.type %in% c("normal", "normal-log")) {
        rate_se <- sqrt(rate_est / tot_person_years)
        rate_ci <- if (conf.type == "normal") {
          rate_est + c(-1, 1) * stats::qnorm(1 - alpha / 2) * rate_se
        } else {
          exp(log(rate_est) + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sqrt(rate_est / tot_person_years) / rate_est)
        }
        conf.low <- rate_ci[1]
        conf.high <- rate_ci[2]
      } else if (conf.type == "exact") {
        conf.low <- stats::qchisq(p = alpha / 2, df = 2 * n_events) / (2 * tot_person_years)
        conf.high <- stats::qchisq(p = 1 - alpha / 2, df = 2 * n_events + 2) / (2 * tot_person_years)
      } else if (conf.type == "byar") {
        seg_1 <- n_events + 0.5
        seg_2 <- 1 - 1 / (9 * (n_events + 0.5))
        seg_3 <- stats::qnorm(1 - alpha / 2) * sqrt(1 / (n_events + 0.5)) / 3
        conf.low <- seg_1 * ((seg_2 - seg_3)^3) / tot_person_years
        conf.high <- seg_1 * ((seg_2 + seg_3)^3) / tot_person_years
      }

      dplyr::tibble(
        estimate = rate_est * n_person_years,
        conf.low = conf.low * n_person_years,
        conf.high = conf.high * n_person_years,
        conf.type = conf.type,
        conf.level = conf.level,
        tot_person_years = tot_person_years,
        n_events = n_events,
        n_unique_id = n_unique_id
      )
    },
    stat_names = c(
      "estimate", "conf.low", "conf.high", "conf.type", "conf.level", "tot_person_years", "n_events", "n_unique_id"
    )
  )

  # build ARD ------------------------------------------------------------------
  cards::ard_complex(
    data = data,
    variables = all_of(time),
    by = any_of(by),
    strata = any_of(strata),
    statistic = all_of(time) ~ list(incidence_rate = calc_incidence_rate)
  ) |>
    dplyr::select(-"stat_label") |>
    dplyr::left_join(
      .df_incidence_rate_stat_labels(n_person_years),
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

.df_incidence_rate_stat_labels <- function(n_person_years) {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "estimate", paste("AE Rate per", n_person_years, "Person-Years"),
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "conf.type", "CI Type",
    "conf.level", "CI Confidence Level",
    "tot_person_years", "Person-Years at Risk",
    "n_events", "Number of AEs Observed",
    "n_unique_id", "Number of Patients with Any AE"
  )
}
