test_that("ard_vif() works", {
  expect_snapshot(
    lm(AGE ~ ARM + SEX, data = cards::ADSL) |>
      ard_vif()
  )
})

test_that("ard_vif() appropriate errors are given for model with only 1 term", {
  expect_error(
    lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_vif()
  )
})
