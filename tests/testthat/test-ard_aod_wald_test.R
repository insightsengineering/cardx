test_that("ard_aod_wald_test() works", {
  # works for a generic case
  expect_error(
    glm_ard_aod_wald_test <-
      suppressWarnings(lm(AGE ~ ARM, data = cards::ADSL)) |>
      ard_aod_wald_test(),
    NA
  )
  expect_equal(nrow(glm_ard_aod_wald_test), 6L)
  expect_snapshot(glm_ard_aod_wald_test [, 1:6])
})
