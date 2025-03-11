#' ARD Incidence Rate
#'
#' Function takes a time variable (`variable`), censoring variable (`cnsr`), and patient ID variable (`id`) and
#' calculates the estimated incidence rate in person years.
#'
#' @param data (`data.frame`)\cr
#'   a data frame. Each row in `data` is assumed to correspond to a unique event occurrence.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of time-to-event variable.
#' @param cnsr ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name used to indicate censoring, i.e. whether an event has been observed (1) or not (0).
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
#'   unit of time of values in `x`.
#'
#'   One of: `years` (default), `months`, `weeks`, or `days`
#' @param n_person_years (`numeric`)\cr
#'   number of person-years to estimate incidence rate for. Defaults to 100.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' cards::ADTTE |>
#'   ard_incidence_rate(variable = AVAL, by = TRTA, cnsr = CNSR, id = USUBJID)
#'
#' cards::ADTTE |>
#'   ard_incidence_rate(variable = AVAL, cnsr = CNSR, id = USUBJID, units = "months", n_person_years = 10)
ard_incidence_rate <- function(data,
                               variable,
                               cnsr,
                               id = NULL,
                               by = NULL,
                               strata = NULL,
                               conf.level = 0.95,
                               conf.type = c("normal", "normal-log", "exact", "byar"),
                               units = c("years", "months", "weeks", "days"),
                               n_person_years = 100) {
    set_cli_abort_call()

    # check inputs ---------------------------------------------------------------
    check_not_missing(data)
    check_not_missing(variable)
    check_not_missing(cnsr)
    check_data_frame(data)
    cards::process_selectors(
      data, variable = {{ variable }}, by = {{ by }}, strata = {{ strata }}, cnsr = {{ cnsr }}, id = {{ id }}
    )
    check_class(data[[variable]], "numeric")
    check_class(data[[cnsr]], "numeric")
    check_range(conf.level, c(0, 1))
    check_numeric(n_person_years)

    conf.type <- arg_match(conf.type, error_call = get_cli_abort_call())
    units <- arg_match(units, error_call = get_cli_abort_call())

    # calculate incidence rate & related statistics ------------------------------
    calc_incidence_rate <- cards::as_cards_fn(
      \(x) {
        tot_person_years <- sum(x, na.rm = TRUE) / (
          (units == "years") + (units == "months") * 12 + (units == "weeks") * 52.14 + (units == "days") * 365.24
        )
        n_events <- sum(data[[cnsr]], na.rm = TRUE)
        n_unique_id <- if (!is.null(id)) {
          sum(!is.na(unique(data[[id]][data[[cnsr]] == 1])))
        } else {
          nrow(data)
        }
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
    cards::ard_continuous(
      data = data,
      variables = all_of(variable),
      by = any_of(by),
      strata = any_of(strata),
      statistic = all_of(variable) ~ list(incidence_rate = calc_incidence_rate)
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
    "estimate", paste("Estimated AE Rate per", n_person_years, "Person-Years"),
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "conf.type", "CI Type",
    "conf.level", "CI Confidence Level",
    "tot_person_years", "Person-Years at Risk",
    "n_events", "Number of AEs Observed",
    "n_unique_id", "Number of Patients with Any AE"
  )
}
