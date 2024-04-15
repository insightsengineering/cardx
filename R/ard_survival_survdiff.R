

ard_survival_survdiff <- function(formula, data, rho = 0, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed(c("survival", "broom"), reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(formula)
  check_class(formula, cls = "formula")
  if (!missing(data)) check_class(data, cls = "data.frame")
  check_scalar(rho)
  check_class(rho, cls = "numeric")

  # assign method
  method <- dplyr::case_when(
    rho == 0 ~ "Log-rank test",
    rho == 1.5 ~ "Tarone-Ware test",
    rho == 1 ~ "Peto & Peto modification of Gehan-Wilcoxon test",
    .default = glue::glue("G-rho test (\U03C1 = {rho})")
  )

  lst_glance <-
    cards::eval_capture_conditions(
      survival::survdiff(formula = formula, data = data, rho = rho, ...) |>
        broom::glance() |>
        dplyr::mutate(method = .env$method)
    )

  # if there was an error, return results early
  if (is.null(lst_glance[["result"]])) {
    variables <- terms(x) |> attr("term.labels")
    # if no variables in formula, then return an error
    if (is_empty(variables)) {
      cli::cli_abort(
        message = lst_glance[["error"]]
      )
    }
    # otherwise, if we do have variable names, then we can construct an empty ARD
    ard_blank_survdiff <-
    .variables_to_survdiff_ard(variables, method) |>
      .add_survdiff_stat_labels() |>
      dplyr::mutate(
        context = "survival_survdiff",
        warning = lst_glance[["warning"]],
        error = lst_glance[["error"]]
      ) |>
      cards::tidy_ard_column_order() %>%
      {structure(., class = c("card", class(.)))} # styler: off
  }
}

.variables_to_survdiff_ard <- function(variables,
                                       method,
                                       stat_names = c("statistic", "df", "p.value"),
                                       stats = list(NULL)) {
  len <- length(variables)

  df_vars <- dplyr::tibble(!!!rev(variables)) |>
    set_names(
      ifelse(
        len > 1L,
        c(paste0("group_", rev(seq_len(len - 1L))), "variable"),
        "variable"
      )
    )

  dplyr::bind_cols(
    df_vars,
    dplyr::tibble(
      stat_name = .env$stat_names,
      stat = .env$stats
    )
  )
}

.add_survdiff_stat_labels <- function(x) {
  x |>
    dplyr::left_join(
      x,
      dplyr::tribble(
        ~stat_name, ~stat_label,
        "statistic", "X^2 Statistic",
        "df", "Degrees of Freedom",
        "p.value", "p-value"
      ),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name))
}
