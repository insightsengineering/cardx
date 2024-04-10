skip_if_not(do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx")))

test_that("ard_moodtest() works", {
  expect_error(
    ard_moodtest <-
      cards::ADSL |>
      ard_moodtest(by = SEX, variable = AGE),
    NA
  )

  expect_equal(
    ard_moodtest |>
      cards::get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    with(cards::ADSL, mood.test(AGE ~ SEX)) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_snapshot(
    cards::ADSL |>
      ard_moodtest(by = SEX, variable = AGE) |>
      as.data.frame()
  )

  expect_equal(
    dplyr::bind_rows(
      ard_moodtest,
      cards::ADSL |>
        ard_moodtest(by = SEX, variable = BMIBL)
    ),
    cards::ADSL |>
      ard_moodtest(by = SEX, variable = c(AGE, BMIBL))
  )
})
