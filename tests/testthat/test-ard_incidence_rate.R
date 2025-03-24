adae <- cards::ADAE |>
  dplyr::rowwise() |>
  dplyr::mutate(time = max(.data$ASTDY, 0))

adtte <- cards::ADTTE

test_that("ard_incidence_rate() works", {
  # default arguments
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, units = "years", count = CNSR, id = USUBJID)
  )
  expect_snapshot(res |> print(columns = "all"))
  expect_equal(res$stat_label[1], "AE Rate per 100 Person-Years")

  adae_single <- adae |>
    dplyr::group_by(USUBJID) |>
    dplyr::summarize(count = dplyr::n(), time = mean(time) * dplyr::n()) # create equal-sized times

  # one row per subject
  expect_silent(
    res <- adae_single |>
      ard_incidence_rate(time = time, units = "days", count = count, id = USUBJID)
  )
  expect_snapshot(res |> print(columns = "all"))

  # multiple rows per subject
  expect_silent(
    res <- adae |>
      ard_incidence_rate(time = time, units = "days")
  )
  expect_snapshot(res |> print(columns = "all"))

  # custom arguments
  expect_silent(
    res <- adae |>
      ard_incidence_rate(time = time, units = "days", id = USUBJID, n_person_time = 50)
  )
  expect_snapshot(res |> print(columns = "all"))
  expect_equal(res$stat_label[1], "AE Rate per 50 Person-Days")
})

test_that("ard_incidence_rate(conf.type) works", {
  # conf.type = "normal-log"
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, units = "years", count = CNSR, id = USUBJID, conf.type = "normal-log")
  )
  expect_snapshot(res |> print(columns = "all"))

  # conf.type = "exact"
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, units = "years", count = CNSR, id = USUBJID, conf.type = "exact")
  )
  expect_snapshot(res |> print(columns = "all"))

  # conf.type = "byar"
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, units = "years", count = CNSR, id = USUBJID, conf.type = "byar")
  )
  expect_snapshot(res |> print(columns = "all"))
})

test_that("ard_incidence_rate() errors are handled correctly", {
  # incorrect conf.type
  expect_snapshot(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, units = "years", count = CNSR, id = USUBJID, conf.type = "standard"),
    error = TRUE
  )

  # incorrect units
  expect_snapshot(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, units = "month", count = CNSR, id = USUBJID),
    error = TRUE
  )
})
