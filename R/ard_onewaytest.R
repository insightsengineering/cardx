#' ARD One-way Test
#'
#' @description
#' Analysis results data for Testing Equal Means in a One-Way Layout.
#' calculated with `oneway.test()`
#'
#' @param x regression model object
#' @param ... additional arguments passed to `oneway.test(...)`
#'
#' @return ARD data frame
#' @export
#'
#' @examples
#' lm(AGE ~ ARM, data = cards::ADSL) |>
#'   ard_onewaytest()
ard_onewaytest <- function(x, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed(c("broom.helpers"), reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(x)

  # build ARD ------------------------------------------------------------------
  onewaytest <-
    cards::eval_capture_conditions(
      stats::oneway.test(x[["call"]][["formula"]], x[["call"]][["data"]])
    )
  onewaytest[["result"]] |>
    broom.helpers::tidy_parameters() |> # using broom.helpers, because it handle non-syntactic names for us
    dplyr::filter(!(dplyr::row_number() == dplyr::n() & .data$term %in% "Residuals")) |> # removing Residual rows
    dplyr::rename(variable = "term") |>
    tidyr::pivot_longer(
      cols = -"variable",
      names_to = "stat_name",
      values_to = "stat"
    ) |>
    dplyr::mutate(
      stat = as.list(.data$stat),
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "statistic" ~ "Statistic",
          .data$stat_name %in% "df" ~ "Degrees of Freedom",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "sumsq" ~ "Sum of Squares",
          .data$stat_name %in% "meansq" ~ "Mean of Sum of Squares",
          TRUE ~ .data$stat_name
        ),
      context = "aov",
      warning = lst_tidy["warning"],
      error = lst_tidy["error"]
    ) |>
    cards::tidy_ard_column_order() %>%
    {
      structure(., class = c("card", class(.)))
    }
}

#   cards::tidy_as_ard(
#     lst_tidy =
#       cards::eval_capture_conditions(
#         stats::oneway.test(x, ...) |>
#           broom::tidy()
#       ),
#     tidy_result_names = c("num.df", "den.df", "statistic", "p.value", "method"),
#     fun_args_to_record =
#       c("var.equal"),
#     formals = formals(stats::oneway.test),
#     passed_args = dots_list(...),
#     lst_ard_columns = list(context = "Oneway.test")
#   ) |>
#     dplyr::mutate(
#       .after = "stat_name",
#       stat_label =
#         dplyr::case_when(
#           .data$stat_name %in% "num.df" ~ "Degrees of Freedom",
#           .data$stat_name %in% "den.df" ~ "Denominator Degrees of Freedom",
#           .data$stat_name %in% "statistic" ~ "F Statistic",
#           .data$stat_name %in% "p.value" ~ "p-value",
#           .data$stat_name %in% "method" ~ "Method",
#           TRUE ~ .data$stat_name,
#         )
#     )
# }
