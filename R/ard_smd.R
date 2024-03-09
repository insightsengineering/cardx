#' ARD Standardized Mean Difference
#'
#' @description
#' Standardized mean difference calculated via [`smd::smd()`] with `na.rm = TRUE`.
#'
#' @param data (`data.frame`/`survey.design`)\cr
#'   a data frame or object of class 'survey.design'
#'   (typically created with [`survey::svydesign()`]).
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared.
#' @inheritDotParams smd::smd -x -g -w -na.rm
#'
#' @return ARD data frame
#' @export
#'
#' @examplesIf cards::is_pkg_installed("smd", reference_pkg = "cardx")
#' ard_smd(cards::ADSL, by = ARM, variable = AGE, std.error = TRUE)
#' ard_smd(cards::ADSL, by = ARM, variable = AGEGR1, std.error = TRUE)
ard_smd <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("smd", reference_pkg = "cardx")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(by)

  # grab design object if from `survey` ----------------------------------------
  is_survey <- inherits(data, "survey.design")
  if (is_survey) {
    design <- data
    data <- design$variables
  }

  # continue check/process inputs ----------------------------------------------
  check_data_frame(data)
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_scalar(by)
  check_scalar(variable)

  # build ARD ------------------------------------------------------------------
  .format_smd_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions(
        switch(as.character(is_survey),
          "TRUE" = smd::smd(x = data[[variable]], g = data[[by]], w = stats::weights(design), na.rm = TRUE, ...),
          "FALSE" = smd::smd(x = data[[variable]], g = data[[by]], na.rm = TRUE, ...)
        ) |>
          dplyr::select(-any_of("term"))
      ),
    ...
  )
}


.format_smd_results <- function(by, variable, lst_tidy, ...) {
  # build ARD ------------------------------------------------------------------
  ret <-
    cards::tidy_as_ard(
      lst_tidy = lst_tidy,
      tidy_result_names = c("estimate", "std.error"),
      fun_args_to_record = "gref",
      formals = formals(smd::smd)["gref"],
      # removing the `std.error` ARGUMENT (not the result)
      passed_args = dots_list(...) |> utils::modifyList(list(std.error = NULL)),
      lst_ard_columns = list(group1 = by, variable = variable, context = "smd")
    )

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      dplyr::tribble(
        ~stat_name, ~stat_label,
        "estimate", "Standardized Mean Difference",
        "std.error", "Standard Error",
        "gref", "Integer Reference Group Level"
      ),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::tidy_ard_column_order()
}
