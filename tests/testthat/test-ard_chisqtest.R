skip_if_not(cards::is_pkg_installed("broom", reference_pkg = "cardx"))

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

test_that("shuffle_ard fills missing group levels if the group is meaningful", {
  adsl_sub <- cards::ADSL |> dplyr::filter(ARM %in% unique(ARM)[1:2])

  expect_snapshot(
    cards::bind_ard(
      ard_chisqtest(
        data = adsl_sub,
        by = "ARM",
        variable = "AGEGR1"
      ),
      ard_chisqtest(
        data = adsl_sub,
        by = "SEX",
        variable = "AGEGR1"
      )
    ) |>
      cards::shuffle_ard() |>
      as.data.frame()
  )
})
