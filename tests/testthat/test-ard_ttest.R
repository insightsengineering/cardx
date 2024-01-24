test_that("ard_ttest() works", {
  expect_error(
    ard_ttest <-
      ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_ttest(by = ARM, variable = AGE, var.equal = TRUE),
    NA
  )

  expect_equal(
    ard_ttest |>
      get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    t.test(
      AGE ~ ARM,
      data = ADSL |> dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")),
      var.equal = TRUE
    ) |>
      broom::tidy() |>
      dplyr::select(estimate, conf.low, conf.high) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_snapshot(
    ADSL |>
      ard_ttest(by = ARM, variable = AGE, var.equal = TRUE) |>
      as.data.frame()
  )
})
