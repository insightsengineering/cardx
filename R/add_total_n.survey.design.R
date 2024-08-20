#' ARD Total N
#'
#' Returns the total N for a survey object.
#' The placeholder variable name returned in the object is `"..ard_total_n.."`
#'
#' @inheritParams ard_dichotomous.survey.design
#' @inheritParams rlang::args_dots_empty
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examplesIf cardx:::is_pkg_installed("survey", reference_pkg = "cardx")
#' svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
#'
#' ard_total_n(svy_titanic)
ard_total_n.survey.design <- function(data, ...) {
  # process inputs -------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty()

  # calculate total N ----------------------------------------------------------
  data$variables <-
    data$variables |>
    dplyr::mutate(..ard_total_n.. = TRUE)

  data |>
    ard_dichotomous(
      variables = "..ard_total_n..",
      statistic = list(..ard_total_n.. = c("N", "N_unweighted"))
    ) |>
    dplyr::mutate(context = "total_n") |>
    dplyr::select(-cards::all_ard_variables("levels"))
}
