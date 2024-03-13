#' Functions for Calculating Univariate Confidence Intervals
#'
#' Functions to calculate different univariate/single statistic confidence intervals for use in `ard_single_ci()`.
#'
#' @param x vector of numeric values
#' @param conf.level (`numeric`)\cr
#'   a scalar in `(0, 1)` indicating the confidence level.
#'   Default is `0.95`
#' @param use_t (`logical`)\cr
#'   a logical to indicated whether to use normal or t distribution.
#'   Default is t distribution.
#' @return Confidence interval of a mean
#'
#' @name single_ci
#' @examplesIf cards::is_pkg_installed("broom", reference_pkg = "cardx")
#' x <- 1:10
#'
#' single_ci_mean(x, conf.level = 0.9, use_t = TRUE)
#' single_ci_mean(x, conf.level = 0.9, use_t = FALSE)
#' single_ci_sd(x, conf.level = 0.9)
#' single_ci_var(x, conf.level = 0.9)
NULL

#' @describeIn single_ci Calculates the mean confidenence interval by following
#' the usual textbook definition with either the normal or t distribution
#'
#' @param use_t (`logical`)\cr use t-distribution vs normal distribution
#'
#' @export
single_ci_mean  <- function(x, conf.level = 0.95, use_t = TRUE) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_scalar_logical(use_t)

  x <- stats::na.omit(x)

  n <- length(x)
  mu <- mean(x)
  sd <- sd(x)

  crit_value <- ifelse(
    use_t,
    stats::qt((1 + conf.level) / 2, df = n - 1),
    stats::qnorm((1 + conf.level) / 2)
  )

  err <- crit_value * sd(x) / sqrt(n)
  l_ci <- max(mu - err)
  u_ci <- min(mu + err)

  list(
    N = n,
    estimate = mu,
    conf.low = l_ci,
    conf.high = u_ci,
    conf.level = conf.level,
    method =
      glue::glue("Mean Confidence Interval {ifelse(use_t, 'with Student t', 'with Normal')} Distribution")
  )
}


#' @describeIn single_ci Calculates the standard deviation confidence interval by following
#' the usual textbook definition with the chi-sq distribution
#'
#' @export
single_ci_sd  <- function(x, conf.level = 0.95) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))

  x <- stats::na.omit(x)

  n <- length(x)
  sd <- sd(x)
  alpha <- 1 - conf.level
  crit_value_left <- stats::qchisq((1 - (alpha / 2)), df = n - 1)
  crit_value_right <- stats::qchisq(alpha / 2, df = n - 1)

  l_ci <- sqrt((n - 1) * sd^2 / crit_value_left )
  u_ci <- sqrt((n - 1) * sd^2 / crit_value_right )

  list(
    N = n,
    estimate = sd,
    conf.low = l_ci,
    conf.high = u_ci,
    conf.level = conf.level,
    method =
      glue::glue("SD Confidence Interval using chi-sq distribution")
  )
}

#' @describeIn single_ci Calculates the variance deviation confidence interval by following
#' the usual textbook definition with the chi-sq distribution
#'
#' @export
single_ci_var  <- function(x, conf.level = 0.95) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))

  x <- stats::na.omit(x)

  n <- length(x)
  var <- var(x)
  alpha <- 1 - conf.level
  crit_value_left <- stats::qchisq((1 - alpha) / 2, df = n - 1)
  crit_value_right <- stats::qchisq(alpha / 2, df = n - 1)

  l_ci <- (n - 1) * var / crit_value_left
  u_ci <- (n - 1) * var / crit_value_right

  list(
    N = n,
    estimate = var,
    conf.low = l_ci,
    conf.high = u_ci,
    conf.level = conf.level,
    method =
      glue::glue("Variance Confidence Interval using chi-sq distribution")
  )
}
