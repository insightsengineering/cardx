#' Regression VIF ARD
#'
#' Function takes a regression model object and converts it to a ARD
#' structure using the `broom.helpers` package.
#'
#' @param x regression model object
#' See car::vif() for details
#'
#' @return data frame
#' @name ard_vif
#' @rdname ard_vif
#' @export
#'
#' @examples
#' lm(AGE ~ ARM + SEX, data = cards::ADSL) |>
#'   ard_vif()
ard_vif <- function(x) {
  # check installed packages ---------------------------------------------------
  cards::check_pkg_installed("broom.helpers", reference_pkg = "cards")

  # check inputs ---------------------------------------------------------------
  check_not_missing(x, "model")

  temp <- x
  vif <- car::vif(x) |>
    cards::eval_capture_conditions()

  # if vif failed, set result as NULL, error will be kept through eval_capture_conditions()
  if (is.null(vif$result)) {
    vif$result <- dplyr::tibble(variable = names(temp$coefficients)[-1], VIF = list(NULL))
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
      values_to = "statistic"
    ) |>
    dplyr::mutate(
      context = "vif",
      stat_label = ifelse(
        .data$stat_name == "aGVIF",
        "Adjusted GVIF",
        .data$stat_name
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
