skip_if_not(do.call(asNamespace("cards")$is_pkg_installed, list(pkg = c("survey", "broom"), reference_pkg = "cardx")))

test_that("ard_svychisq() works", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)

  expect_error(
    ard_svychisq <-
      ard_svychisq(
        dclus2,
        variables = sch.wide,
        by = comp.imp,
        statistic = "F"
      ),
    NA
  )

  expect_equal(
    cards::get_ard_statistics(
      ard_svychisq,
      stat_name %in% c("statistic", "p.value")
    ),
    survey::svychisq(~ sch.wide + comp.imp, dclus2)[c("statistic", "p.value")],
    ignore_attr = TRUE
  )

  # test that the function works with multiple variables
  expect_snapshot(
    ard_svychisq(
      dclus2,
      variables = c(sch.wide, stype),
      by = comp.imp,
      statistic = "adjWald"
    ) |>
      dplyr::select(c(1:3, 5:6)) |>
      dplyr::group_by(variable) |>
      dplyr::slice_head(n = 3) |>
      as.data.frame()
  )


  expect_equal(
    dplyr::bind_rows(
      ard_svychisq,
      dclus2 |>
        ard_svychisq(by = comp.imp, variables = stype)
    ),
    dclus2 |>
      ard_svychisq(by = comp.imp, variables = c(sch.wide, stype))
  )
})
