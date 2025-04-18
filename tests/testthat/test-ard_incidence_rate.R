adae <- cards::ADAE |>
  dplyr::rowwise() |>
  dplyr::mutate(time = max(.data$ASTDY, 0))

adtte <- cards::ADTTE

test_that("ard_incidence_rate() works", {
  # default arguments
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, count = CNSR, id = USUBJID, unit_label = "years")
  )
  expect_snapshot(res |> print(columns = "all"))
  expect_equal(res$stat_label[1], "Incidence rate per 100 Person-Years")

  adae_single <- adae |>
    dplyr::group_by(USUBJID) |>
    dplyr::summarize(count = dplyr::n(), time = mean(time) * dplyr::n()) # create equal-sized times

  # one row per subject
  expect_silent(
    res <- adae_single |>
      ard_incidence_rate(time = time, count = count, id = USUBJID, unit_label = "days")
  )
  expect_snapshot(res |> print(columns = "all"))

  # multiple rows per subject
  expect_silent(
    res <- adae |>
      ard_incidence_rate(time = time, unit_label = "days")
  )
  expect_snapshot(res |> print(columns = "all"))

  # custom arguments
  expect_silent(
    res <- adae |>
      ard_incidence_rate(time = time, id = USUBJID, n_person_time = 50, unit_label = "days")
  )
  expect_snapshot(res |> print(columns = "all"))
  expect_equal(res$stat_label[1], "Incidence rate per 50 Person-Days")
})

test_that("ard_incidence_rate(conf.type) works", {
  # conf.type = "normal-log"
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, count = CNSR, id = USUBJID, unit_label = "years", conf.type = "normal-log")
  )
  expect_snapshot(res |> print(columns = "all"))

  # conf.type = "exact"
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, count = CNSR, id = USUBJID, unit_label = "years", conf.type = "exact")
  )
  expect_snapshot(res |> print(columns = "all"))

  # conf.type = "byar"
  expect_silent(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, count = CNSR, id = USUBJID, unit_label = "years", conf.type = "byar")
  )
  expect_snapshot(res |> print(columns = "all"))
})

test_that("ard_incidence_rate() errors are handled correctly", {
  # incorrect time
  expect_snapshot(
    res <- adtte |>
      ard_incidence_rate(time = SEX, count = CNSR, id = USUBJID),
    error = TRUE
  )

  # incorrect unit_label
  expect_snapshot(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, count = CNSR, id = USUBJID, unit_label = 10),
    error = TRUE
  )

  # incorrect conf.type
  expect_snapshot(
    res <- adtte |>
      ard_incidence_rate(time = AVAL, count = CNSR, id = USUBJID, unit_label = "years", conf.type = "standard"),
    error = TRUE
  )
})
