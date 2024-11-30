skip_if_not(is_pkg_installed("broom"))

test_that("ard_stats_mantelhaen_test.data.frame() works", {
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

  # function works with multiple variables
  expect_equal(
    dplyr::bind_rows(
      ard_mantelhaentest,
      cards::ADSL |>
        ard_stats_mantelhaen_test(by = ARM, variables = BMIBLGR1, strata = SEX)
    ),
    cards::ADSL |>
      ard_stats_mantelhaen_test(by = ARM, variables = c(AGEGR1, BMIBLGR1), strata = SEX)
  )
})

test_that("ard_stats_mantelhaen_test.array() works", {
  penicillin <- array(
    c(0, 0, 6, 5, 3, 0, 3, 6, 6, 2, 0, 4, 5, 6, 1, 0, 2, 5, 0, 0), dim = c(2, 2, 5),
    dimnames = list(
      Delay = c("None", "1.5h"), Response = c("Cured", "Died"), Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4")
    )
  )

  expect_silent(
    ard_mantelhaentest <- penicillin |>
      ard_stats_mantelhaen_test()
  )

  # error messages for incorrect array structure
  bad_array <- array(
    c(0, 0, 6, 5, 3, 0, 3, 6, 6, 2, 0, 4, 5, 6, 1, 0, 2, 5, 0, 0), dim = c(4, 5),
    dimnames = list(
      Delay = c("None", "1.5h", "Cured", "Died"), Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4")
    )
  )
  expect_snapshot(
    ard_mantelhaentest <- bad_array |>
      ard_stats_mantelhaen_test(),
    error = TRUE
  )
  bad_array <- array(
    c(0, 0, 6, 5, 3, 0, 3, 6, 6, 2, 0, 4, 5, 6, 1, 0, 2, 5, 0, 0), dim = c(2, 2, 5)
  )
  expect_snapshot(
    ard_mantelhaentest <- bad_array |>
      ard_stats_mantelhaen_test(),
    error = TRUE
  )
})

test_that("ard_stats_mantelhaen_test() follows ard structure", {
  expect_silent(
    cards::ADSL |>
      ard_stats_mantelhaen_test(by = ARM, variables = AGEGR1, strata = SEX) |>
      cards::check_ard_structure()
  )
})
