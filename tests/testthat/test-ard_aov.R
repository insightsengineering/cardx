test_that("ard_aov() works", {
  expect_error(
    ard_aov <-
      lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_aov(),
    NA
  )

  expect_equal(
    ard_aov |>
      cards::get_ard_statistics(stat_name %in% c("sumsq", "statistic")),
    aov(
      AGE ~ ARM,
      data = cards::ADSL
    ) |>
      broom::tidy() |>
      dplyr::slice_head() |>
      dplyr::select(sumsq, statistic) |>
      unclass(),
    ignore_attr = TRUE
  )

  # see if it can handle multiple variables
  expect_snapshot(
    lm(AGE ~ ARM + SEX, data = cards::ADSL) |>
      ard_aov() |>
      as.data.frame()
  )
})
