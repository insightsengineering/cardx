test_that("ard_chisqtest() works", {
  expect_error(
    ard_chisqtest <-
      cards::ADSL |>
      ard_chisqtest(by = ARM, variable = AGEGR1),
    NA
  )

  expect_equal(
    ard_chisqtest |>
      cards::get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    with(cards::ADSL, chisq.test(AGEGR1, ARM)) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )
})
