test_that("ard_incidence_rate() works", {
  # default arguments
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID)
  )
  expect_snapshot(res)

  # custom arguments
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, var_unit = "month", n_person_years = 10)
  )
  expect_snapshot(res)
})

test_that("ard_incidence_rate(conf.type) works", {
  # conf.type = "normal-log"
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, conf.type = "normal-log")
  )
  expect_snapshot(res)

  # conf.type = "exact"
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, conf.type = "exact")
  )
  expect_snapshot(res)

  # conf.type = "byar"
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, conf.type = "byar")
  )
  expect_snapshot(res)
})

test_that("ard_incidence_rate() errors are handled correctly", {
  # incorrect conf.type
  expect_snapshot(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, conf.type = "standard"),
    error = TRUE
  )

  # incorrect var_unit
  expect_snapshot(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, var_unit = "months"),
    error = TRUE
  )
})
