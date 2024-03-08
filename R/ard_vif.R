#' Regression VIF ARD
#'
#' @description
#' Function takes a regression model object and returns the variance inflation factor (VIF)
#' using [`car::vif()`] and converts it to a ARD structure
#'
#' @param x regression model object
#' See car::vif() for details
#'
#' @param ... arguments passed to `car::vif(...)`
#'
#' @return data frame
#' @name ard_vif
#' @rdname ard_vif
#' @export
#'
#' @examples
#' lm(AGE ~ ARM + SEX, data = cards::ADSL) |>
#'   ard_vif()
ard_vif <- function(x, ...) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(x, "model")

  vif <- car::vif(x, ...) |>
    cards::eval_capture_conditions()

  # if vif failed, set result as NULL, error will be kept through eval_capture_conditions()
  if (is.null(vif$result)) {
    vif$result <- dplyr::tibble(
      variable = attr(terms(x), "term.labels"),
      VIF = list(NULL),
      GVIF = list(NULL),
      aGVIF = list(NULL),
      df = list(NULL)
    )
  }
  # if VIF is returned
  else if (!is.matrix(vif$result)) {
    vif$result <- dplyr::tibble(variable = names(vif$result), VIF = vif$result)
  }
  # if Generalized VIF is returned
  else if (is.matrix(vif$result)) {
    vif$result <-
      vif$result |>
      as.data.frame() %>%
      dplyr::mutate(., variable = rownames(.), .before = 1L) |>
      dplyr::rename(
        aGVIF = "GVIF^(1/(2*Df))",
        df = "Df"
      ) |>
      dplyr::tibble()
  }

  # Clean-up the result to fit the ard structure through pivot
  vif$result <-
    vif$result |>
    tidyr::pivot_longer(
      cols = -c("variable"),
      names_to = "stat_name",
      values_to = "stat"
    ) |>
    dplyr::mutate(
      context = "vif",
      stat_label = ifelse(
        .data$stat_name == "aGVIF",
        "Adjusted GVIF",
        .data$stat_name
      ),
      fmt_fn = map(
        .data$stat,
        function(.x) {
          # styler: off
          if (is.integer(.x)) return(0L)
          if (is.numeric(.x)) return(1L)
          # styler: on
          NULL
        }
      )
    )

  # Bind the results and possible warning/errors together
  vif_return <- dplyr::tibble(
    vif$result,
    warning = vif["warning"],
    error = vif["error"]
  )

  # Clean up return object
  vif_return |>
    cards::tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))}  # styler: off
}
