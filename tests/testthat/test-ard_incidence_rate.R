test_that("ard_incidence_rate() works", {
  # default arguments
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID)
  )
  expect_snapshot(res |> print(columns = "all"))

  # custom arguments
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, units = "months", n_person_years = 10)
  )
  expect_snapshot(res |> print(columns = "all"))
})

test_that("ard_incidence_rate(conf.type) works", {
  # conf.type = "normal-log"
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, conf.type = "normal-log")
  )
  expect_snapshot(res |> print(columns = "all"))

  # conf.type = "exact"
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, conf.type = "exact")
  )
  expect_snapshot(res |> print(columns = "all"))

  # conf.type = "byar"
  expect_silent(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, conf.type = "byar")
  )
  expect_snapshot(res |> print(columns = "all"))
})

test_that("ard_incidence_rate() errors are handled correctly", {
  # incorrect conf.type
  expect_snapshot(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, conf.type = "standard"),
    error = TRUE
  )

  # incorrect units
  expect_snapshot(
    res <- cards::ADTTE |>
      ard_incidence_rate(AVAL, CNSR, USUBJID, units = "month"),
    error = TRUE
  )
})
