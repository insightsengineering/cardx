test_that("ard_aod_waldtest() works", {
  # works for a generic case
  expect_error(
    glm_ard_aod_waldtest <-
      suppressWarnings(lm(AGE ~ ARM, data = cards::ADSL)) |>
      ard_aod_waldtest(),
    NA
  )
  expect_equal(nrow(glm_ard_aod_waldtest), 6L)
  expect_snapshot(glm_ard_aod_waldtest[, 1:6])

  # works when a regression model isn't passed

  expect_snapshot(
    ard_aod_waldtest(cards::ADSL) |>
      dplyr::select(c(context, error))
  )
})
