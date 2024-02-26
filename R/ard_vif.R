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

  vif_return <- .vif_to_tibble(x)
  dplyr::tibble(
    vif_return$result,
    warning = vif_return["warning"],
    error = vif_return["error"]
  )
}


# put VIF results in data frame -- borrowed from gtsummary
.vif_to_tibble <- function(x) {
  temp <- x
  vif <- car::vif(x) |>
    cards::eval_capture_conditions()

  # if VIF is returned
  if (is.null(vif$result)) {
    vif$result <- dplyr::tibble(variable = names(temp$coefficients)[-1], VIF = list(NULL))
  } else if (!is.matrix(vif$result)) {
    vif$result <- dplyr::tibble(variable = names(vif$result), VIF = vif$result)
  } # if Generalized VIF is returned
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

  vif$result <-
    vif$result |>
    tidyr::pivot_longer(
      cols = -c("variable"),
      names_to = "stat_name",
      values_to = "statistic"
    ) |>
    dplyr::mutate(
      .after = "variable",
      context = "vif"
    ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label = ifelse(
        stat_name == "aGVIF",
        "Adjusted GVIF",
        stat_name
      )
    )

  return(vif)
}
