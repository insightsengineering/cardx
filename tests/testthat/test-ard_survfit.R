test_that("ard_survfit() works with times provided", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(times = c(60, 180)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survfit() works with reverse=TRUE", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(times = c(60, 180), reverse = TRUE) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survfit() works with probs provided", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(probs = c(0.25, 0.75), reverse = TRUE) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survfit() errors are properly handled", {
  expect_snapshot(
    ard_survfit("not_survfit"),
    error = TRUE
  )

  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(times = 100, probs = c(0.25, 0.75)),
    error = TRUE
  )
})
