

ard_svy_continuous <- function(data, variables, by = NULL,
                               statistic = everything() ~ c("mean", "median"),
                               fmt_fn = NULL, stat_label = NULL) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_class(data, class = "survey.design")
  check_not_missing(variables)

  # process inputs -------------------------------------------------------------
  cards::process_selectors(data$variables, variables = {{ variables }}, by = {{ by }})
  variables <- setdiff(variables, by)
  cards::process_formula_selectors(data$variables[variables], statistic = statistic)

}


compute_survey_stat <- function(data, variable, by, f) {
  # difftime variable needs to be transformed into numeric for svyquantile
  if (inherits(data$variables[[variable]], "difftime")) {
    data$variables[[variable]] <- unclass(data$variables[[variable]])
  }

  args <- list(design = data, na.rm = TRUE, keep.var = FALSE)

  # if all values are NA, turn na.rm to FALSE to avoid error
  if (all(is.na(data$variables[[variable]]))) {
    args$na.rm <- FALSE
  }

  args <-
    args |>
    append(
      switch(
        f,
        "mean" = list(FUN = survey::svymean),
        "sum" = list(FUN = survey::svytotal),
        "var" = list(FUN = survey::svyvar),
        "sd" = list(FUN = \(...) survey::svyvar(...) |> sqrt()),
        "mean.std.error" = list(FUN = \(...) survey::svymean(...) |> survey::SE()),
        "deff" = list(FUN = \(...) survey::svymean(..., deff = TRUE) |> survey::deff()),
        "median" = list(FUN = survey::svyquantile, quantiles = 0.5),
        "min" = list(FUN = \(x, design, ...) min(design$variables[[all.vars(x)]], na.rm = na.rm)),
        "max" = list(FUN = \(x, design, ...) max(design$variables[[all.vars(x)]], na.rm = na.rm))
      )
    )

  if (f %in% paste0("p", 0:100)) {
    args <-
      args |>
      append(
        list(FUN = survey::svyquantile, quantiles = substr(f, 2, nchar(f)) |> as.numeric())
      )
  }

  if (is.null(by)) {
    args$x <- c_form(right = variable)
    stat <- do.call(fun, args)
    stat <- tibble(variable, stat[1]) %>%
      set_names(c("variable", f))
  } else {
    args$formula <- c_form(right = variable)
    args$by <- c_form(right = by)
    args$FUN <- fun
    stat <- do.call(survey::svyby, args)
    stat <- stat %>%
      as_tibble() %>%
      select(1:2) %>%
      set_names(c("by", f)) %>%
      mutate(variable = variable)
  }

  if (f == "sd") {
    stat$sd <- sqrt(stat$sd)
  }

  stat
}
