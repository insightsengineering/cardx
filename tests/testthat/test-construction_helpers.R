skip_if_not(is_pkg_installed(c("broom.helpers", "withr", "survey"), reference_pkg = "cardx"))

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

  expect_equal(
    {
      outside_fun <- function() {
        method.args <- list()

        construct_model.data.frame(
          mtcars,
          formula = mpg ~ cyl,
          method = "lm",
          method.args = method.args
        ) |>
          coef()
      }

      outside_fun()
    },
    lm(mpg ~ cyl, mtcars) |> coef()
  )

  # now the survey method -------
  # styler: off
  expect_equal({
    data(api, package = "survey")
    # stratified sample
    survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc) |>
      construct_model(formula = api00 ~ api99, method = "svyglm") |>
      ard_regression() |>
      cards::get_ard_statistics(stat_name %in% "estimate")},
    survey::svyglm(
      api00 ~ api99,
      design = survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
    ) |>
      coef() |>
      getElement(2L) |>
      list(estimate = _)
  )
  # styler: on
})
