test_that("ard_mcnemartest() works", {
  expect_error(
    ard_mcnemartest <-
      cards::ADSL |>
      ard_mcnemartest(by = SEX, variable = EFFFL),
    NA
  )

  expect_equal(
    ard_mcnemartest |>
      cards::get_ard_statistics(stat_name %in% c("statistic", "p.value", "parameter", "method")),
    stats::mcnemar.test(cards::ADSL[["SEX"]], cards::ADSL[["EFFFL"]], correct = TRUE) |>
      broom::tidy() |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_equal(
    cards::ADSL |>
      ard_mcnemartest(by = ARM, variable = AGE, correct = FALSE) |>
      dplyr::pull(error) |>
      getElement(1L),
    "'x' and 'y' must have the same number of levels (minimum 2)"
  )

  # non-syntactic column names work too
  ADSL_tmp <- cards::ADSL |>
    dplyr::rename("if" = AGE, "_c d" = EFFFL)

  expect_error(
    ard_mcnemartest <-
      ADSL_tmp |>
      ard_mcnemartest(by = `if`, variable = `_c d`),
    NA
  )
})
