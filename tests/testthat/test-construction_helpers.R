test_that("construct_model() works", {
  expect_snapshot(
    construct_model(
      x = mtcars |> dplyr::rename(`M P G` = mpg),
      formula = reformulate2(c("M P G", "cyl"), response = "hp"),
      method = "lm"
    ) |>
      ard_regression() |>
      dplyr::filter(stat_name %in% c("term", "estimate", "p.value"))
  )
})
