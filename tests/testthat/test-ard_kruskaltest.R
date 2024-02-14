test_that("ard_kurskaltest() works", {
  expect_error(
    ard_kruskaltest <-
      cards::ADSL |>
      ard_kruskaltest(by = ARM, variable = AGE),
    NA
  )

  expect_equal(
    ard_kruskaltest |>
      cards::get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    with(cards::ADSL, kruskal.test(AGE, ARM)) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_snapshot(
    cards::ADSL |>
      ard_kruskaltest(by = "ARM", variable = "AGE") |>
      as.data.frame()
  )
})
