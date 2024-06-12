

ard_categorical.survey.design <- function(data,
                                          variables,
                                          by = NULL,
                                          statistic = everything() ~ c("n", "N", "p"),
                                          denominator = c("column", "row", "cell"),
                                          fmt_fn = NULL,
                                          stat_label = everything() ~ cards::default_stat_labels(),
                                          ...) {
  set_cli_abort_call()

  # process arguments ----------------------------------------------------------
  cards::process_selectors(
    data = data$variables,
    variables = {{ variables }},
    by = {{ by }}
  )
  cards::process_formula_selectors(
    data = data$variables[variables],
    statistic = statistic,
    fmt_fn = fmt_fn,
    stat_label = stat_label
  )
  denominator <- arg_match(denominator)

  # calculate counts -----------------------------------------------------------
  lst_svytable_counts <-
    lapply(
      variables,
      \(variable) {
        survey::svytable(formula = reformulate2(c(by, variable)))
      }
    )

}
