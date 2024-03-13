test_that("ard_single_ci() works", {
  # testing the easy methods together
  expect_error(
    c(
      "mean_with_Z", "mean_with_T", "sd", "var"
    ) |>
      lapply(
        \(x) {
          ard_single_ci(
            data = cards::ADSL,
            variables = AGE,
            method = x
          )
        }
      ),
    NA
  )
})

test_that("ard_single_ci(method='mean_with_T') works", {
  ard_single_mean_T <- ard_single_ci(cards::ADSL, variables = c(AGE), method = "mean_with_T")
  expect_snapshot(ard_single_mean_T)
})

test_that("ard_single_ci(method='mean_with_Z') works", {
  ard_single_mean_Z <- ard_single_ci(cards::ADSL, variables = c(AGE), method = "mean_with_Z")
  expect_snapshot(ard_single_mean_Z)
})

test_that("ard_single_ci(method='sd') works", {
  ard_single_sd <- ard_single_ci(cards::ADSL, variables = c(AGE), method = "sd")
  expect_snapshot(ard_single_sd)
})

test_that("ard_single_ci(method='var') works", {
  ard_single_var <- ard_single_ci(cards::ADSL, variables = c(AGE), method = "var")
  expect_snapshot(ard_single_var)
})

