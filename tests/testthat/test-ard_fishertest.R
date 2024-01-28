test_that("ard_fishertest() works", {
  expect_error(
    ard_fishertest <-
      cards::ADSL[1:20, ] |>
      ard_fishertest(by = ARM, variable = AGEGR1),
    NA
  )

  expect_equal(
    ard_fishertest |>
      cards::get_ard_statistics(stat_name %in% c("p.value", "method")),
    with(cards::ADSL[1:20, ], fisher.test(AGEGR1, ARM)) |>
      broom::tidy() |>
      dplyr::select(p.value, method) |>
      unclass(),
    ignore_attr = TRUE
  )
})

