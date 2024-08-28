skip_if_not(do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "boot", reference_pkg = "cardx")))

test_that("boot_ci() works with standard use", {
  x <- cards::ADSL$AGE

  set.seed(1)
  res <- boot_ci(x, type = "perc")
  expect_snapshot(res)

  set.seed(1)
  res <- cards::ADSL |>
    cards::ard_continuous(
      variables = AGE,
      statistic = everything() ~ list(boot_ci = boot_ci)
    )

  expect_snapshot(res)
})

test_that("boot_ci() warnings work", {
  x <- cards::ADSL$AGE

  expect_snapshot(
    boot_ci(x[1], type = "perc")
  )
})
