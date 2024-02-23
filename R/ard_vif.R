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

  .vif_to_tibble(x) |>
    tidyr::pivot_longer(
      cols = -c("variable", "row_type"),
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
}


# put VIF results in data frame -- borrowed from gtsummary
.vif_to_tibble <- function(x) {
  vif <- tryCatch(
    car::vif(x),
    error = function(e) {
      paste(
        "The {.code add_vif()} uses {.code car::vif()} to",
        "calculate the VIF, and the function returned an error (see below)."
      ) |>
        stringr::str_wrap() |>
        cli::cli_alert_danger()
      stop(e)
    }
  )

  # if VIF is returned
  if (!is.matrix(vif)) {
    result <-
      vif |>
      tibble::enframe("variable", "VIF")
  } # if Generalized VIF is returned
  else {
    result <-
      vif |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "variable") |>
      tibble::as_tibble() |>
      dplyr::rename(
        aGVIF = "GVIF^(1/(2*Df))",
        df = "Df"
      )
  }

  result <-
    result |>
    dplyr::mutate(
      variable = broom.helpers::.clean_backticks(.data$variable),
      row_type = "label"
    )
  return(result)
}
