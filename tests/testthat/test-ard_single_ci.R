test_that("ard_single_ci() works", {
  # testing the easy methods together
  expect_error(
    c(
      "mean_with_Z", "mean_with_T"
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
