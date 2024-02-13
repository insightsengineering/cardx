test_that("ard_regression() works", {
  expect_snapshot(
    lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_regression(add_estimate_to_reference_rows = TRUE) |>
      dplyr::mutate(
        statistic = lapply(statistic, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      as.data.frame()
  )
})
