test_that("check the single_ci_*() functions work", {
  # setting vectors to test
  x_dbl <- c(NA, 1:10)

  # check mean CIs ----------------------------------------------------------
  expect_snapshot(
    single_ci_mean(x_dbl, conf.level = 0.9, use_t = TRUE)
  )

  expect_snapshot(
    single_ci_mean(x_dbl, conf.level = 0.9, use_t = FALSE)
  )
})
