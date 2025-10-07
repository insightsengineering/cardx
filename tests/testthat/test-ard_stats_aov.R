skip_if_pkg_not_installed(c("broom.helpers", "parameters", "withr"))

test_that("ard_aov() works", {
  withr::local_options(list(width = 180))
  expect_error(
    ard_aov <-
      ard_stats_aov(AGE ~ ARM, data = cards::ADSL),
    NA
  )

  expect_equal(
    ard_aov |>
      cards::get_ard_statistics(stat_name %in% c("sumsq", "statistic")),
    aov(
      AGE ~ ARM,
      data = cards::ADSL
    ) |>
      broom::tidy() |>
      dplyr::slice_head() |>
      dplyr::select(sumsq, statistic) |>
      unclass(),
    ignore_attr = TRUE
  )

  # see if it can handle multiple variables
  expect_snapshot(
    ard_stats_aov(AGE ~ ARM + SEX, data = cards::ADSL) |>
      as.data.frame()
  )
})

test_that("ard_stats_aov() follows ard structure", {
  expect_silent(
    ard_stats_aov(AGE ~ ARM, data = cards::ADSL) |>
      cards::check_ard_structure(method = FALSE)
  )
})
