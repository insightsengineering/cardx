test_that("ard_onewaytest() works", {
  expect_error(
    ard_onewaytest <-
      cards::ADSL |>
      ard_onewaytest(by = ARM, variable = AGE),
    NA
  )

  expect_equal(
    ard_onewaytest |>
      cards::get_ard_statistics(stat_name %in% c("num.df", "statistic", "method")),
    oneway.test(
      AGE ~ ARM,
      data = cards::ADSL
    ) |>
      broom::tidy() |>
      dplyr::select(num.df, statistic, method) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_snapshot(
    cards::ADSL |>
      ard_onewaytest(by = ARM, variable = AGEGR1) |>
      as.data.frame()
  )
})
