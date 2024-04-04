test_that("ard_onewaytest() works", {
  expect_error(
    ard_onewaytest <- ard_onewaytest(AGE ~ ARM, data = cards::ADSL),
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

  # warnings are properly handled - "variable" should be continuous, not factor
  expect_snapshot(
    ard_onewaytest(AGEGR1 ~ ARM, data = cards::ADSL) |>
      dplyr::select(c("stat_name", "warning")) |>
      as.data.frame() |>
      head(3)

  )
})
