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
    with(cards::ADSL, kruskal.test(AGEGR1, ARM)) |>
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
      ard_kruskaltest(
        data = adsl_sub,
        by = "ARM",
        variable = "AGE"
      ),
      ard_kruskaltest(
        data = adsl_sub,
        by = "SEX",
        variable = "AGE"
      )
    ) |>
      as.data.frame()
  )
})
