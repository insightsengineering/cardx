skip_if_not(cards::is_pkg_installed("broom.helpers", reference_pkg = "cardx"))

test_that("ard_regression() works", {
  expect_snapshot(
    lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_regression(add_estimate_to_reference_rows = TRUE) |>
      as.data.frame() |>
      dplyr::select(-context, -stat_label, -fmt_fn) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      )
  )

  # checking non-syntactic names
  expect_equal(
    lm(AGE ~ `Treatment Arm`, data = cards::ADSL |> dplyr::rename(`Treatment Arm` = ARM)) |>
      ard_regression(add_estimate_to_reference_rows = TRUE) |>
      dplyr::pull(variable) |>
      unique(),
    "Treatment Arm"
  )
})
