skip_if_not(is_pkg_installed("broom"))

test_that("ard_stats_mantelhaen_test() works", {
  withr::local_options(width = 200)

  expect_silent(
    ard_mantelhaentest <- cards::ADSL |>
      ard_stats_mantelhaen_test(by = ARM, variable = AGEGR1, strata = SEX)
  )

  expect_snapshot(ard_mantelhaentest |> print(columns = "all"))

  expect_equal(
    ard_mantelhaentest |>
      cards::get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    with(cards::ADSL, mantelhaen.test(AGEGR1, ARM, SEX)) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )

  # custom arguments to stats::mantelhaen.test()
  expect_silent(
    ard_mantelhaentest <- cards::ADSL |>
      ard_stats_mantelhaen_test(
        by = ARM, variable = AGEGR1, strata = SEX,
        alternative = "less", correct = FALSE, exact = TRUE, conf.level = 0.90
      )
  )

  expect_snapshot(ard_mantelhaentest |> print(columns = "all"))
})
