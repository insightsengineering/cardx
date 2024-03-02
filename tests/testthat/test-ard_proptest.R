test_that("ard_proptest() works", {
  expect_error(
    ard_proptest <-
      mtcars |>
      ard_proptest(by = vs, variable = am, conf.level = 0.90),
    NA
  )

  expect_equal(
    ard_proptest |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "estimate1", "estimate2", "conf.low", "conf.high")),
    prop.test(
      x = c(mtcars$am[mtcars$vs == 0] |> sum(),
            mtcars$am[mtcars$vs == 1] |> sum()),
      n = c(sum(mtcars$vs == 0), sum(mtcars$vs == 1)),
      conf.level = 0.90
    ) |>
      broom::tidy() |>
      dplyr::mutate(estimate = estimate1 - estimate2, .before = 1L) |>
      dplyr::select(starts_with("estimate"), conf.low, conf.high) |>
      unclass(),
    ignore_attr = TRUE
  )
})

test_that("ard_proptest() error messaging", {
  # the AGE column is not binary and we should get an error captured
  expect_error(
    non_binary <-
      cards::ADSL |>
      ard_proptest(by = ARM, variable = AGE) |>
      as.data.frame(),
    NA
  )
  expect_equal(nrow(non_binary), 13L)
  expect_equal(
    non_binary$error |> unique() |> cli::ansi_strip(),
    "Expecting `variable` to be either <logical> or <numeric/integer> coded as 0 and 1."
  )

  # passing a by variable with 3 levels (only 2 is allowed)
  expect_error(
    too_many_levels <-
      mtcars |>
      ard_proptest(by = cyl, variable = vs) |>
      as.data.frame(),
    NA
  )
  expect_equal(nrow(too_many_levels), 13L)
  expect_equal(
    too_many_levels$error |> unique() |> cli::ansi_strip(),
    "The `by` column must have exactly 2 levels.\nThe levels are 4, 6, and 8"
  )
})
