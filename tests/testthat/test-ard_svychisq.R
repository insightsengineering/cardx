skip_if_not(cards::is_pkg_installed(c("survey", "broom"), reference_pkg = "cardx"))

test_that("ard_svychisq() works", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)

  expect_error(
    ard_svychisq <-
      ard_svychisq(
        dclus2,
        variable = sch.wide,
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
    survey::svychisq(~sch.wide + comp.imp, dclus2)[c("statistic", "p.value")],
    ignore_attr = TRUE
  )
})
