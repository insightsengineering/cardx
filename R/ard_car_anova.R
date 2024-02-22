#' Anova ARD from car Package
#'
#' Function takes a regression model object and calculated ANOVA using `car::Anova()`
#'
#' @param x regression model object
#' @param ... arguments passed to `car::Anova(...)`
#'
#' @return data frame
#'
#' @examples
#' lm(AGE ~ ARM, data = cards::ADSL) |>
#'   ard_car_anova()
ard_car_anova <- function(x, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("car", reference_pkg = "cardx")
  cards::check_pkg_installed("broom.helpers", reference_pkg = "cardx")

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)

  # run car::Anova() -----------------------------------------------------------
  car::Anova() |>
    broom.helpers::tidy_parameters(conf.int = FALSE) |> # using broom.helpers, because it handle non-syntactic names for us
    dplyr::rename(variable = "term") |>
    tidyr::pivot_longer(
      cols = -"variable",
      names_to = "stat_name",
      values_to = "stat"
    ) |>
    dplyr::mutate(
      stat = as.list(stat),
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "statistic" ~ "Statistic",
          .data$stat_name %in% "df" ~ "Degrees of Freedom",
          .data$stat_name %in% "p.value" ~ "p-value",
          TRUE ~ .data$stat_name
        ),
      fmt_fn =
        map(
          .data$stat,
          function(.x) {
            if (is.numeric(.x)) return(1L)
            if (is.integer(.x)) return(0L)
            NULL
          }
        )
    ) |>
    cards::tidy_ard_column_order()
}
