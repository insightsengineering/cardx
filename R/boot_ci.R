#' Function for Calculating Nonparametric Bootstrap Confidence Intervals
#'
#' This function first applies [boot::boot()] to convert the input data `x` to a `boot` object and then uses
#' [boot::boot.ci()] to calculate the corresponding nonparametric bootstrap confidence interval.
#'
#' @param x (`numeric`)\cr vector of numeric values, i.e. a numeric vector, or a logical vector which will be
#'   interpreted as binary with values `c(0, 1)`.
#' @param conf.level (`numeric`)\cr a scalar in `(0, 1)` indicating the confidence level. Default is `0.95`.
#' @param R (`integer`)\cr the number of bootstrap replicates. See the parameter description in [boot::boot()]
#'   for details.
#' @param type (`character`)\cr a vector of character strings representing the type of intervals required. The value
#'   should be any subset of the values `c("norm","basic", "stud", "perc", "bca")` or simply `"all"` which will
#'   compute all five types of intervals.
#' @param stype (`string`)\cr a character string indicating what the second argument of statistic represents.
#'   Possible values of stype are `"i"` (indices - the default), `"f"` (frequencies), or `"w"` (weights).
#' @param statistic (`function`)\cr a function which, when applied to data, returns a vector containing the
#'   statistic(s) of interest. See the parameter description in [boot::boot()] for details.
#'
#' @return Bootstrap confidence interval.
#' @export
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "boot", reference_pkg = "cardx"))
#' cards::ADSL |>
#'   cards::ard_continuous(
#'     variables = AGE,
#'     statistic = everything() ~ list(boot_ci = boot_ci)
#'   )
boot_ci <- function(x,
                    conf.level = 0.95,
                    R = 2000,
                    type = "all",
                    stype = "i",
                    statistic = \(x, i) mean(x[i], na.rm = TRUE)) {
  set_cli_abort_call()

  # check installed packages ---------------------------------------------------
  check_pkg_installed(pkg = "boot", reference_pkg = "cardx")

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_class(x, cls = c("numeric", "logical"))
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_scalar(conf.level)
  check_class(R, cls = "numeric")
  check_string(type)
  check_string(stype)
  check_class(statistic, cls = "function")

  x <- stats::na.omit(x)

  result <- list(
    N = NULL,
    estimate = NULL,
    conf.low = NULL,
    conf.high = NULL,
    conf.level = conf.level,
    R = R,
    ci.type = type
  )

  boot <- boot::boot(data = x, statistic = statistic, R = R, stype = stype)
  result$N <- boot$data |> length()
  result$estimate <- boot$t0

  if (length(x) <= 1) {
    cli::cli_warn(
      paste0("All values of t are equal to ", boot$t0, ". Cannot calculate confidence intervals."),
      call = get_cli_abort_call()
    )
  } else {
    boot.ci <- boot::boot.ci(boot, conf = conf.level, type = type)
    result$conf.low <- boot.ci$percent[1, 4] |> unname()
    result$conf.high <- boot.ci$percent[1, 5] |> unname()
  }

  result
}
