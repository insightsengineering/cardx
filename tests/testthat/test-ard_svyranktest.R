skip_if_not(cards::is_pkg_installed(c("survey", "broom"), reference_pkg = "cardx"))

test_that("ard_svyranktest() works", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)

  expect_error(
    svyranktest <- lapply(
      c("wilcoxon", "vanderWaerden", "median","KruskalWallis"),
      function(x) {
        ard_svyranktest(
          dclus2,
          variable = enroll,
          by = comp.imp,
          test = x
        )
      }),
    NA
  )
})

test_that("ard_svyranktest() snapshots", {
  expect_snapshot(svyranktest[[1]] |> as.data.frame())
  expect_snapshot(svyranktest[[2]] |> as.data.frame())
  expect_snapshot(svyranktest[[3]] |> as.data.frame())
  expect_snapshot(svyranktest[[4]] |> as.data.frame())
})

test_that("exact values match for ard_svyranktest works", {
  svywilcox <- ard_svyranktest(
    dclus2,
    variable = enroll,
    by = comp.imp,
    test = "wilcoxon"
  )
  expect_equal(
    cards::get_ard_statistics(
      svywilcox,
      stat_name %in% c("estimate", "p.value")
    ),
    survey::svyranktest(enroll ~ comp.imp, dclus2, test = "wilcoxon")[c("estimate", "p.value")],
    ignore_attr = TRUE
  )
})
