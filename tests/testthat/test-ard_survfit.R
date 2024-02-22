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

test_that("ard_survfit() works with probs provided", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(probs = c(0.25, 0.75)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})
