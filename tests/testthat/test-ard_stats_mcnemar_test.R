skip_if_not(cards::is_pkg_installed("broom", reference_pkg = "cardx"))

test_that("ard_stats_mcnemar_test() works", {
  expect_error(
    ard_mcnemartest <-
      cards::ADSL |>
      ard_stats_mcnemar_test(by = SEX, variables = EFFFL),
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
      ard_stats_mcnemar_test(by = ARM, variables = AGE, correct = FALSE) |>
      dplyr::pull(error) |>
      getElement(1L),
    "'x' and 'y' must have the same number of levels (minimum 2)"
  )

  # non-syntactic column names work too
  ADSL_tmp <- cards::ADSL |>
    dplyr::rename("if" = AGE, "_c d" = EFFFL)

  expect_equal(
    cards::ADSL |>
      dplyr::rename(`Planned Tx` = TRT01P, `Age Group` = AGEGR1) |>
      ard_stats_mcnemar_test(by = `Planned Tx`, variables = `Age Group`) |>
      cards::get_ard_statistics(),
    cards::ADSL |>
      ard_stats_mcnemar_test(by = TRT01P, variables = AGEGR1) |>
      cards::get_ard_statistics()
  )

  # test that the function works with multiple variables
  expect_equal(
    dplyr::bind_rows(
      ard_mcnemartest,
      cards::ADSL |>
        ard_stats_mcnemar_test(by = SEX, variables = COMP16FL)
    ),
    cards::ADSL |>
      ard_stats_mcnemar_test(by = SEX, variables = c(EFFFL, COMP16FL))
  )
})
