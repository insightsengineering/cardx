test_that("ard_vif() works", {
  expect_snapshot(
    lm(AGE ~ ARM + SEX, data = cards::ADSL) |>
      ard_vif() |>
      as.data.frame()
  )

  expect_snapshot(
    lm(AGE ~ BMIBL + EDUCLVL, data = cards::ADSL) |>
      ard_vif() |>
      as.data.frame()
  )
})

test_that("ard_vif() appropriate errors are given for model with only 1 term", {
  expect_snapshot(
    lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_vif() |>
      as.data.frame()
  )
  expect_equal(
    lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_vif() |>
      dplyr::select(error) |>
      unlist() |>
      unique(),
    "model contains fewer than 2 terms"
  )
})


test_that("ard_vif() issues friendly messaging for incorrect object passed in/can't get terms of model", {
  expect_snapshot(
    try(cards::ADSL |> ard_vif())
  )
})

