adae <- cards::ADAE |>
  dplyr::rowwise() |>
  dplyr::mutate(interval = max(.data$ASTDY, 0))

adtte <- cards::ADTTE

test_that("ard_incidence_rate() works", {
  # default arguments
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(interval = AVAL, count = CNSR, id = USUBJID)
  )
  expect_snapshot(res |> print(columns = "all"))
  expect_equal(res$stat_label[1], "AE Rate per 100 Person-Years")

  adae_single <- adae |>
    dplyr::group_by(USUBJID) |>
    dplyr::summarize(count = dplyr::n(), interval = mean(interval))

  # one row per subject
  expect_silent(
    res <- adae_single |>
      ard_incidence_rate(interval = interval, count = count, id = USUBJID, units = "days")
  )
  expect_snapshot(res |> print(columns = "all"))

  # multiple rows per subject
  expect_silent(
    res <- adae |>
      ard_incidence_rate(interval = interval, units = "days")
  )
  expect_snapshot(res |> print(columns = "all"))

  # custom arguments
  expect_silent(
    res <- adae |>
      ard_incidence_rate(interval = interval, id = USUBJID, units = "days", n_person_years = 50)
  )
  expect_snapshot(res |> print(columns = "all"))
  expect_equal(res$stat_label[1], "AE Rate per 50 Person-Years")
})

test_that("ard_incidence_rate(conf.type) works", {
  # conf.type = "normal-log"
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(interval = AVAL, count = CNSR, id = USUBJID, conf.type = "normal-log")
  )
  expect_snapshot(res |> print(columns = "all"))

  # conf.type = "exact"
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(interval = AVAL, count = CNSR, id = USUBJID, conf.type = "exact")
  )
  expect_snapshot(res |> print(columns = "all"))

  # conf.type = "byar"
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(interval = AVAL, count = CNSR, id = USUBJID, conf.type = "byar")
  )
  expect_snapshot(res |> print(columns = "all"))
})

test_that("ard_incidence_rate() errors are handled correctly", {
  # incorrect conf.type
  expect_snapshot(
    res <- adtte |>
      ard_incidence_rate(interval = AVAL, count = CNSR, id = USUBJID, conf.type = "standard"),
    error = TRUE
  )

  # incorrect units
  expect_snapshot(
    res <- adtte |>
      ard_incidence_rate(interval = AVAL, count = CNSR, id = USUBJID, units = "month"),
    error = TRUE
  )
})
