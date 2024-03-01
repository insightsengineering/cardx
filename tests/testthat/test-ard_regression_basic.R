test_that("ard_regression_basic() works", {
  expect_error(
    ard <- lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_regression_basic(),
    NA
  )

  expect_snapshot(as.data.frame(ard) |> dplyr::select(-fmt_fn))
})
