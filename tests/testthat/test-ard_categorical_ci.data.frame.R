test_that("ard_categorical_ci.data.frame(method = 'wald')", {
  skip_if_not(is_pkg_installed("broom"))

  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "wald"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_wald(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'waldcc')", {
  skip_if_not(is_pkg_installed("broom"))

  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "waldcc"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_wald(mtcars[["vs"]], correct = TRUE) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'clopper-pearson')", {
  skip_if_not(is_pkg_installed("broom"))

  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "clopper-pearson"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_clopper_pearson(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'wilson')", {
  skip_if_not(is_pkg_installed("broom"))

  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "wilson"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_wilson(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'wilsoncc')", {
  skip_if_not(is_pkg_installed("broom"))

  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "wilsoncc"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_wilson(mtcars[["vs"]], correct = TRUE) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'strat_wilson')", {
  skip_if_not(is_pkg_installed("broom"))

  mtcars$gear <- as.factor(mtcars$gear)
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      strata = "gear",
      method = "strat_wilson"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_strat_wilson(mtcars[["vs"]], strata = as.factor(mtcars[["gear"]])) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'strat_wilsoncc')", {
  skip_if_not(is_pkg_installed("broom"))

  mtcars$gear <- as.factor(mtcars$gear)
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      strata = "gear",
      method = "strat_wilsoncc"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_strat_wilson(mtcars[["vs"]], strata = as.factor(mtcars[["gear"]]), correct = TRUE) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'jeffreys')", {
  skip_if_not(is_pkg_installed("broom"))

  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "jeffreys"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_jeffreys(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'agresti-coull')", {
  skip_if_not(is_pkg_installed("broom"))

  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "agresti-coull"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_agresti_coull(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_continuous_ci.data.frame() follows ard structure", {
  skip_if_not(is_pkg_installed("broom"))

  expect_silent(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "wald"
    ) |>
      cards::check_ard_structure()
  )
})
