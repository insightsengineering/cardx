

ard_survival_survdiff <- function(data, response, variables, rho = 0, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed(c("survival", "broom"), reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(outcome)
  check_class(data, cls = "data.frame")
  cards::process_selectors(data, variables = {{ variables }})
  response <- enexpr(response) |> expr_deparse() # create a string of the outcome
  formula <- stats::reformulate(variables, response = response)

  lst_glance <-
    cards::eval_capture_conditions(
      survival::survdiff(formula = x, data = data, rho = rho, ...) |>
        broom::glance() |>
        dplyr::mutate(
          method =
            dplyr::case_when(
              rho == 0 ~ "Log-rank test",
              rho == 1.5 ~ "Tarone-Ware test",
              rho == 1 ~ "Peto & Peto modification of Gehan-Wilcoxon test",
              .default = glue::glue("G-rho test (\U03C1 = {rho})")
            )
        )
    )

  # if there was an error, return results early
  # if (is.null(lst_glance[["result"]])) {
  #   variables <- terms(x) |> attr("term.labels")
  #   if (is_empty(variables))
  #
  # }
}
