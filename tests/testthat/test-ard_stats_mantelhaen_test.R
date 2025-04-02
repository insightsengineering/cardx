skip_if_not(is_pkg_installed("broom"))

test_that("ard_stats_mantelhaen_test() works", {
  withr::local_options(width = 200)

  expect_silent(
    ard_mantelhaentest <- cards::ADSL |>
      ard_stats_mantelhaen_test(by = ARM, variables = AGEGR1, strata = SEX)
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

test_that("ard_stats_mantelhaen_test() displays errors correctly", {
  data <- cards::ADSL |>
    dplyr::mutate(ARM = "ARM A")

  expect_silent(
    ard_mantelhaentest <- data |>
      ard_stats_mantelhaen_test(by = ARM, variables = AGEGR1, strata = SEX)
  )
  expect_equal(nrow(ard_mantelhaentest), 9)
  expect_equal(ard_mantelhaentest$error, as.list(rep("'x' and 'y' must have at least 2 levels", 9)))
})
