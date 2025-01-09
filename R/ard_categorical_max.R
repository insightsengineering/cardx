#' ARD to Calculate Categorical Occurrence Rates by Maximum Level Per Unique ID
#'
#' Function calculates categorical variable level occurrences rates by maximum level per unique ID.
#' Each variable in `variables` is evaluated independently and then results for all variables are stacked.
#' Only the highest-ordered level will be counted for each unique ID.
#' Unordered, non-numeric variables will be converted to factor and the default level order used for ordering.
#'
#' @inheritParams cards::ard_categorical
#' @inheritParams cards::ard_stack
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The categorical variables for which occurrence rates per unique ID (by maximum level) will be calculated.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Argument used to subset `data` to identify rows in `data` to calculate categorical variable level occurrence rates.
#' @param denominator (`data.frame`, `integer`)\cr
#'   Used to define the denominator and enhance the output.
#'   The argument is optional. If not specified, `data` will be used as `denominator`.
#'   - the univariate tabulations of the `by` variables are calculated with `denominator` when a data frame is passed,
#'     e.g. tabulation of the treatment assignment counts that may appear in the header of a table.
#' @param quiet (scalar `logical`)\cr
#'   Logical indicating whether to suppress additional messaging. Default is `FALSE`.
#'
#' @return an ARD data frame of class 'card'
#' @name ard_categorical_max
#'
#' @examples
#' # Occurrence Rates by Max Level (Highest Severity) --------------------------
#' ard_categorical_max(
#'   cards::ADAE,
#'   variables = c(AESER, AESEV),
#'   id = USUBJID,
#'   by = TRTA,
#'   denominator = cards::ADSL |> dplyr::rename(TRTA = ARM)
#' )
NULL

#' @rdname ard_categorical_max
#' @export
ard_categorical_max <- function(data,
                                variables,
                                id,
                                by = dplyr::group_vars(data),
                                statistic = everything() ~ c("n", "p", "N"),
                                denominator = NULL,
                                fmt_fn = NULL,
                                stat_label = everything() ~ cards::default_stat_labels(),
                                quiet = TRUE,
                                ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(id)
  cards::process_selectors(data, variables = {{ variables }}, id = {{ id }}, by = {{ by }})
  data <- dplyr::ungroup(data)

  # denominator must a data frame, or integer
  if (!is_empty(denominator) && !is.data.frame(denominator) && !is_integerish(denominator)) {
    cli::cli_abort(
      "The {.arg denominator} argument must be a {.cls data.frame} or an {.cls integer}, not {.obj_type_friendly {denominator}}.",
      call = get_cli_abort_call()
    )
  }
  if (is_empty(denominator)) denominator <- data

  # check the id argument is not empty
  if (is_empty(id)) {
    cli::cli_abort("Argument {.arg id} cannot be empty.", call = get_cli_abort_call())
  }

  # return empty ARD if no variables selected ----------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble() |> cards::as_card())
  }

  # order variables
  for (v in variables) {
    if (is.character(data[[v]])) {
      lvls <- .unique_and_sorted(data[[v]])
      vec <- cli::cli_vec(lvls, style = list("vec-sep" = " < ", "vec-sep2" = " < ", "vec-last" = " < "))
      if (!quiet) {
        cli::cli_inform(
          paste(
            "The {.var {v}} variable is {.obj_type_friendly {data[[v]]}}. It has been converted to a {.cls factor} variable with",
            "the following ordered levels: {.val {vec}}."
          )
        )
      }
      data[[v]] <- factor(data[[v]], levels = lvls, ordered = TRUE)
    }
  }

  # drop missing values --------------------------------------------------------
  df_na_nan <- is.na(data[c(by, variables)]) | apply(data[c(by, variables)], MARGIN = 2, is.nan)
  if (any(df_na_nan)) {
    rows_with_na <- apply(df_na_nan, MARGIN = 1, any)
    cli::cli_inform(c("*" = "Removing {.val {sum(rows_with_na)}} row{?s} from {.arg data} with
                            {.val {NA}} or {.val {NaN}} values in {.val {c(by, variables)}} column{?s}."))
    data <- data[!rows_with_na, ]
  }

  # remove missing by variables from `denominator`
  if (is.data.frame(denominator) && !is_empty(intersect(by, names(denominator)))) {
    df_na_nan_denom <-
      is.na(denominator[intersect(by, names(denominator))]) |
        apply(denominator[intersect(by, names(denominator))], MARGIN = 2, is.nan)
    if (any(df_na_nan_denom)) {
      rows_with_na_denom <- apply(df_na_nan_denom, MARGIN = 1, any)
      cli::cli_inform(c("*" = "Removing {.val {sum(rows_with_na_denom)}} row{?s} from {.arg denominator} with
                              {.val {NA}} or {.val {NaN}} values in {.val {intersect(by, names(denominator))}} column{?s}."))
      denominator <- denominator[!rows_with_na_denom, ]
    }
  }

  # sort data ------------------------------------------------------------------
  data <- dplyr::arrange(data, dplyr::pick(all_of(c(id, by, variables))))

  # print denom columns if not 100% clear which are used
  if (!is_empty(id) && is.data.frame(denominator)) {
    denom_cols <- intersect(by, names(denominator))
    if (!setequal(by, denom_cols)) {
      msg <-
        ifelse(
          is_empty(denom_cols),
          "Denominator set by number of rows in {.arg denominator} data frame.",
          "Denominator set by {.val {denom_cols}} column{?s} in {.arg denominator} data frame."
        )
      cli::cli_inform(c("i" = msg))
    }
  }

  lst_results <- list()
  for (var in variables) {
    lst_results <-
      lst_results |>
      append(
        ard_categorical(
          data = data |>
            dplyr::slice_tail(n = 1L, by = all_of(c(id, intersect(by, names(denominator))))),
          variables = all_of(var),
          by = all_of(by),
          statistic = statistic,
          denominator = denominator,
          fmt_fn = fmt_fn,
          stat_label = stat_label
        ) |>
          list()
      )

    lst_results[[length(lst_results)]] <- lst_results[[length(lst_results)]] |>
      dplyr::mutate(variable_level = as.list(as.character(unlist(.data$variable_level))))
  }

  # combine results ------------------------------------------------------------
  result <- lst_results |>
    dplyr::bind_rows() |>
    dplyr::mutate(context = "categorical_max") |>
    cards::tidy_ard_column_order() |>
    cards::tidy_ard_row_order()

  # return final result --------------------------------------------------------
  result
}
