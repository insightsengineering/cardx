#' ARD Incidence Rate
#'
#' Function takes a time variable (`variable`), censoring variable (`cnsr`), and patient ID variable (`id`) and
#' calculates the estimated incidence rate in person years.
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of time-to-event variable.
#' @param cnsr ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name used to indicate censoring, i.e. whether an event has been observed (1) or not (0).
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name used to identify unique patients in `data`.
#' @param `conf.level` (`numeric`)\cr
#'   confidence level for the estimated incidence rate.
#' @param `conf.type` (`string`)\cr
#'   confidence interval type for the estimated incidence rate.
#'
#'   One of: `normal` (default), `normal_log`, `exact`, or `byar`.
#' @param `var_unit` (`string`)\cr
#'   unit of time of values in `x`.
#'
#'   One of: `day`, `week`, `month`, or `year` (default)
#' @param `n_person_years` (`numeric`)\cr
#'   number of person-years to estimate incidence rate for. Defaults to 100.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' cards::ADTTE |>
#'   ard_incidence_rate(AVAL, CNSR, USUBJID)
#'
#' cards::ADTTE |>
#'   ard_incidence_rate(AVAL, CNSR, USUBJID, var_unit = "month", n_person_years = 10)
ard_incidence_rate <- cards::as_cards_fn(
  function(data,
           variable,
           cnsr,
           id,
           conf.level = 0.95,
           conf.type = "normal",
           var_unit = "year",
           n_person_years = 100) {
    set_cli_abort_call()

    # check inputs ---------------------------------------------------------------
    check_not_missing(data)
    check_not_missing(variable)
    check_not_missing(cnsr)
    check_not_missing(id)
    check_data_frame(data)
    cards::process_selectors(data, variable = {{ variable }}, cnsr = {{ cnsr }}, id = {{ id }})
    check_range(conf.level, c(0, 1))
    check_string(conf.type)
    check_string(var_unit)
    check_numeric(n_person_years)
    if (!conf.type %in% c("normal", "normal_log", "exact", "byar")) {
      cli::cli_abort(
        paste(
          "The {.arg conf.type} argument is {.val {conf.type}} but must be one of",
          "{.val {c('normal', 'normal_log', 'exact', 'byar')}}."
        ),
        call = get_cli_abort_call()
      )
    }
    if (!var_unit %in% c("day", "week", "month", "year")) {
      cli::cli_abort(
        paste(
          "The {.arg var_unit} argument is {.val {var_unit}} but must be one of",
          "{.val {c('day', 'week', 'month', 'year')}}."
        ),
        call = get_cli_abort_call()
      )
    }

    # calculate incidence rate & related statistics ------------------------------
    tot_person_years <- sum(data[[variable]], na.rm = TRUE) / (
      (var_unit == "year") + (var_unit == "month") * 12 + (var_unit == "week") * 52.14 + (var_unit == "day") * 365.24
    )
    n_events <- sum(data[[cnsr]], na.rm = TRUE)
    n_unique_id <- sum(!is.na(unique(data[[id]][data[[cnsr]] == 1])))
    rate_est <- n_events / tot_person_years
    alpha <- 1 - conf.level
    if (conf.type %in% c("normal", "normal_log")) {
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

    # build ARD ------------------------------------------------------------------
    result <- dplyr::tibble(
      estimate = rate_est * n_person_years,
      estimate_label = paste("Estimated AE rate per", n_person_years, "person-years"),
      conf.low = conf.low * n_person_years,
      conf.high = conf.high * n_person_years,
      conf.type = conf.type,
      conf.level = conf.level,
      tot_person_years = tot_person_years,
      n_events = n_events,
      n_unique_id = n_unique_id
    )

    dplyr::tibble(
      variable = variable,
      stat_name = names(result),
      stat = as.list(result),
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "conf.low" ~ "CI Lower Bound",
          .data$stat_name %in% "conf.high" ~ "CI Upper Bound",
          .data$stat_name %in% "conf.type" ~ "CI Type",
          .data$stat_name %in% "conf.level" ~ "CI Confidence Level",
          .data$stat_name %in% "tot_person_years" ~ "Person-Years at Risk",
          .data$stat_name %in% "n_events" ~ "Number of AEs Observed",
          .data$stat_name %in% "n_unique_id" ~ "Number of Patients with Any AE",
          TRUE ~ .data$stat_name
        ),
      context = "incidence_rate"
    ) |>
    cards::as_card() |>
    cards::tidy_ard_row_order() |>
    cards::tidy_ard_column_order()
  },
  stat_names = c(
    "estimate", "conf.low", "conf.high", "conf.type", "conf.level", "tot_person_years", "n_events"
  )
)
