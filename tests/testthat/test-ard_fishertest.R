skip_if_not(do.call(asNamespace("cards")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx")))

test_that("ard_fishertest() works", {
  expect_error(
    ard_fishertest <-
      cards::ADSL[1:20, ] |>
      ard_fishertest(by = ARM, variables = AGEGR1),
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

  # function works with multiple variables
  expect_equal(
    dplyr::bind_rows(
      ard_fishertest,
      cards::ADSL[1:20, ] |>
        ard_fishertest(by = ARM, variables = BMIBLGR1)
    ),
    cards::ADSL[1:20, ] |>
      ard_fishertest(by = ARM, variables = c(AGEGR1, BMIBLGR1))
  )
})
