#' ARD Cohen's D Test
#'
#' @description
#' Analysis results data for paired and non-paired Cohen's D Effect Size Test
#' using `effectsize::cohens_d()`.
#'
#' @param data (`data.frame`)\cr
#'   a data frame. See below for details.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to compare by. Must be a categorical variable with exactly two levels.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name to be compared. Must be a continuous variable.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column name of the subject or participant ID
#' @param ... arguments passed to `effectsize::cohens_d(...)`
#'
#' @return ARD data frame
#' @name ard_cohensd
#'
#' @details
#' For the `ard_cohensd()` function, the data is expected to be one row per subject.
#' The data is passed as `effectsize::cohens_d(data[[variable]]~data[[by]], data, paired = FALSE, ...)`.
#'
#' For the `ard_paired_cohensd()` function, the data is expected to be one row
#' per subject per by level. Before the effect size is calculated, the data are
#' reshaped to a wide format to be one row per subject.
#' The data are then passed as
#' `effectsize::cohens_d(x = data_wide[[<by level 1>]], y = data_wide[[<by level 2>]], paired = TRUE, ...)`.
#'
#' @examples
#' cards::ADSL |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   ard_cohensd(by = ARM, variable = AGE)
#'
#' # constructing a paired data set,
#' # where patients receive both treatments
#' cards::ADSL[c("ARM", "AGE")] |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
#'   dplyr::arrange(USUBJID, ARM) |>
#'   dplyr::group_by(USUBJID) |>
#'   dplyr::filter(dplyr::n() > 1) |>
#'   ard_paired_cohensd(by = ARM, variable = AGE, id = USUBJID)
NULL

#' @rdname ard_cohensd
#' @export
ard_cohensd <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("effectsize", reference_pkg = "cardx")
  cards::check_pkg_installed("parameters", reference_pkg = "cardx")
  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(by)
  check_data_frame(data)
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_scalar(by)
  check_scalar(variable)

  # build ARD ------------------------------------------------------------------
  .format_cohensd_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions(
        effectsize::cohens_d(data[[variable]] ~ data[[by]], data = data, paired = FALSE, ...) |>
          parameters::standardize_names(style = "broom")
      ),
    paired = FALSE,
    ...
  )
}


#' @rdname ard_cohensd
#' @export
ard_paired_cohensd <- function(data, by, variable, id, ...) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("effectsize", reference_pkg = "cardx")
  cards::check_pkg_installed("parameters", reference_pkg = "cardx")
  # check/process inputs -------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(by)
  check_not_missing(id)
  check_data_frame(data)
  data <- dplyr::ungroup(data)
  cards::process_selectors(data, by = {{ by }}, variable = {{ variable }}, id = {{ id }})
  check_scalar(by)
  check_scalar(variable)
  check_scalar(id)

  # build ARD ------------------------------------------------------------------
  .format_cohensd_results(
    by = by,
    variable = variable,
    lst_tidy =
      cards::eval_capture_conditions({
        # adding this reshape inside the eval, so if there is an error it's captured in the ARD object
        data_wide <- .paired_data_pivot_wider(data, by = by, variable = variable, id = id)
        # perform paired cohen's d test
        effectsize::cohens_d(x = data_wide[["by1"]], y = data_wide[["by2"]], paired = TRUE, ...) |>
          parameters::standardize_names(style = "broom")
      }),
    paired = TRUE,
    ...
  )
}

.df_effectsize_stat_labels <- function() {
  dplyr::tribble(
    ~stat_name, ~stat_label,
    "estimate", "Effect Size Estimate",
    "conf.low", "CI Lower Bound",
    "conf.high", "CI Upper Bound",
    "conf.level", "CI Confidence Level",
  )
}


#' Convert Cohen's D Test to ARD
#'
#' @inheritParams cards::tidy_as_ard
#' @inheritParams effectsize::cohens_d
#' @param by (`string`)\cr by column name
#' @param variable (`string`)\cr variable column name
#' @param ... passed to `cohens_d(...)`
#'
#' @return ARD data frame
#' @keywords internal
#' @examples
#' cardx:::.format_cohensd_results(
#'   by = "ARM",
#'   variable = "AGE",
#'   paired = FALSE,
#'   lst_tidy =
#'     cards::eval_capture_conditions(
#'       effectsize::hedges_g(data[[variable]] ~ data[[by]], paired = FALSE) |>
#'         parameters::standardize_names(style = "broom")
#'     )
#' )
.format_cohensd_results <- function(by, variable, lst_tidy, paired, ...) {
  # build ARD ------------------------------------------------------------------
  ret <-
    cards::tidy_as_ard(
      lst_tidy = lst_tidy,
      tidy_result_names = c(
        "estimate", "conf.level", "conf.low", "conf.high"
      ),
      fun_args_to_record = c("mu", "paired", "pooled_sd"),
      formals = formals(asNamespace("effectsize")[["cohens_d"]]),
      passed_args = c(list(paired = paired), dots_list(...)),
      lst_ard_columns = list(group1 = by, variable = variable, context = "cohensd")
    )

  # add the stat label ---------------------------------------------------------
  ret |>
    dplyr::left_join(
      .df_effectsize_stat_labels(),
      by = "stat_name"
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)) |>
    cards::tidy_ard_column_order()
}
