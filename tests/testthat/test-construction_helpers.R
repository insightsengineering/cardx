test_that("construct_model() works", {
  expect_snapshot(
    construct_model(
      x = mtcars |> dplyr::rename(`M P G` = mpg),
      formula = reformulate2(c("M P G", "cyl"), response = "hp"),
      method = "lm"
    ) |>
      ard_regression() |>
      as.data.frame() |>
      dplyr::filter(stat_name %in% c("term", "estimate", "p.value"))
  )

  expect_equal(
    mtcars[c("mpg", "hp", "vs")] |>
      dplyr::rename(`M P G` = mpg, `h\np` = hp) |>
      names() |>
      bt(),
    c("`M P G`", "`h\np`", "vs")
  )

  expect_equal(
    bt_strip(c("`complex variable name`", "east_variable_name")),
    c("complex variable name", "east_variable_name")
  )

  expect_error(
    check_not_namespaced("geepack::geeglm"),
    "cannot be namespaced"
  )
})
