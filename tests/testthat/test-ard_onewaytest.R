test_that("ard_onewaytest() works", {
  expect_error(
    ard_onewaytest <- lm(AGE ~ ARM, data = cards::ADSL) |>
         ard_onewaytest(),
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

  # errors are properly handled - "variable" should be continuous, not factor
  expect_snapshot(
    lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_onewaytest()|>
      dplyr::select(c("stat_name", "error")) |>
      as.data.frame()
  )
})
