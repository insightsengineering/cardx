test_that("ard_aov() works", {
  expect_error(
    ard_aov <-
      cards::ADSL |>
      ard_aov(by = ARM, variable = AGE),
    NA
  )

  expect_equal(
    ard_aov |>
      cards::get_ard_statistics(stat_name %in% c("term", "sumsq", "statistic")),
    aov(
      AGE ~ ARM,
      data = cards::ADSL
    ) |>
      broom::tidy() |>
      dplyr::slice_head() |>
      dplyr::select(term, sumsq, statistic) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_snapshot(
    cards::ADSL |>
      ard_aov(by = ARM, variable = AGEGR1) |>
      as.data.frame()
  )
})
