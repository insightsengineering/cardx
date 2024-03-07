skip_if_not(cards::is_pkg_installed("smd", reference_pkg = "cardx"))

test_that("ard_smd() works", {
  expect_error(
    ard_smd <-
      mtcars |>
      ard_smd(by = vs, variable = am, std.error = TRUE),
    NA
  )

  expect_equal(
    ard_smd |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "std.error")),
    smd::smd(x = mtcars$am, g = mtcars$vs, std.error = TRUE) |>
      dplyr::select(-term) |>
      unclass(),
    ignore_attr = TRUE
  )
})

test_that("ard_proptest() error messaging", {
  # mis-specify the gref argument
  expect_error(
    bad_gref <-
      ard_smd(cards::ADSL, by = ARM, variable = AGE, std.error = TRUE, gref = 0) |>
      as.data.frame(),
    NA
  )
  # check all the stats still appear despite the errors
  expect_equal(nrow(bad_gref), 3L)
  expect_setequal(bad_gref$stat_name, c("estimate", "std.error", "gref"))
  # check the error message it the one we expect
  expect_equal(
    bad_gref$error |> unique() |> cli::ansi_strip(),
    "gref must be an integer within 3"
  )
})
