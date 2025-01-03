skip_if_not(is_pkg_installed("broom"))

test_that("ard_categorical_ci.data.frame(method = 'wald')", {
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

  # check `denominator = "row"` result
  expect_equal(
    ard_categorical_ci(
      mtcars |> dplyr::mutate(cyl = factor(cyl)),
      variables = vs,
      by = am,
      strata = "cyl",
      method = "strat_wilson",
      denominator = "row"
    ) |>
      dplyr::filter(group1_level %in% 0) |>
      cards::get_ard_statistics(),
    proportion_ci_strat_wilson(
      x = (mtcars$am == 0)[mtcars$vs == 1],
      strata = as.factor(mtcars[["cyl"]])[mtcars$vs == 1]
    )
  )

  # check `denominator = "cell"` result
  expect_equal(
    ard_categorical_ci(
      mtcars |> dplyr::mutate(cyl = factor(cyl)),
      variables = vs,
      by = am,
      strata = "cyl",
      method = "strat_wilson",
      denominator = "cell"
    ) |>
      dplyr::filter(group1_level %in% 0, variable_level %in% 1) |>
      cards::get_ard_statistics(),
    proportion_ci_strat_wilson(
      x = mtcars$am == 0 & mtcars$vs == 1,
      strata = as.factor(mtcars[["cyl"]])
    )
  )
})

test_that("ard_categorical_ci.data.frame(method = 'strat_wilsoncc')", {
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
  expect_silent(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "wald"
    ) |>
      cards::check_ard_structure()
  )
})

test_that("ard_continuous_ci.data.frame(denominator='row')", {
  # check the structure of the output
  expect_silent(
    ard <- ard_categorical_ci(
      mtcars,
      by = am,
      variables = vs,
      value = NULL,
      denominator = "row"
    )
  )
  expect_silent(
    cards::check_ard_structure(ard)
  )

  # check the estimates align with `cards::ard_categorical(denominator='row)`
  expect_equal(
    ard |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level)),
    cards::ard_categorical(
      mtcars,
      by = am,
      variables = vs,
      denominator = "row",
      statistic = ~"p"
    ) |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level))
  )

  # check the estimates align with `cards::ard_categorical(denominator='row)` for dichotomous variables
  expect_equal(
    ard_categorical_ci(
      mtcars,
      by = am,
      variables = vs,
      denominator = "row"
    ) |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level)),
    cards::ard_dichotomous(
      mtcars,
      by = am,
      variables = vs,
      denominator = "row",
      statistic = ~"p"
    ) |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level))
  )
})

test_that("ard_continuous_ci.data.frame(denominator='cell')", {
  # check the structure of the output
  expect_silent(
    ard <- ard_categorical_ci(
      mtcars,
      by = am,
      variables = vs,
      value = NULL,
      denominator = "cell"
    )
  )
  expect_silent(
    cards::check_ard_structure(ard)
  )

  # check the estimates align with `cards::ard_categorical(denominator='row)`
  expect_equal(
    ard |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(group1, variable, unlist(group1_level), unlist(variable_level)),
    cards::ard_categorical(
      mtcars,
      by = am,
      variables = vs,
      denominator = "cell",
      statistic = ~"p"
    ) |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(group1, variable, unlist(group1_level), unlist(variable_level))
  )

  # check the estimates align with `cards::ard_categorical(denominator='row)` for dichotomous variables
  expect_equal(
    ard_categorical_ci(
      mtcars,
      by = am,
      variables = vs,
      denominator = "cell"
    ) |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level)),
    cards::ard_dichotomous(
      mtcars,
      by = am,
      variables = vs,
      denominator = "cell",
      statistic = ~"p"
    ) |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level))
  )
})
