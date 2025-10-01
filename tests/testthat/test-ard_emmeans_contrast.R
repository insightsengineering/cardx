skip_if_not(is_pkg_installed(c("emmeans", "survey", "lme4")))

test_that("ard_emmeans_contrast() works", {
  withr::local_options(width = 250)

  # estimate_type = difference
  expect_silent(
    ard_emmeans_contrast <-
      ard_emmeans_contrast(
        data = mtcars,
        formula = vs ~ am + mpg,
        method = "glm",
        method.args = list(family = binomial),
        response_type = "dichotomous"
      )
  )
  expect_snapshot(ard_emmeans_contrast |> print(columns = "all"))

  expect_equal(
    cards::get_ard_statistics(ard_emmeans_contrast, stat_name %in% "method")[1],
    list(method = "Least-squares adjusted mean difference")
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_contrast, stat_name %in% "estimate") |>
      unlist() |>
      unname(),
    glm(vs ~ am + mpg, data = mtcars, family = binomial) |>
      emmeans::emmeans(specs = ~am, regrid = "response") |>
      emmeans::contrast(method = "pairwise") |>
      summary(infer = TRUE) |>
      getElement("estimate")
  )

  # estimate_type = emmean
  expect_silent(
    ard_emmeans_mean_estimates <-
      ard_emmeans_contrast(
        data = mtcars,
        formula = vs ~ am + mpg,
        method = "glm",
        method.args = list(family = binomial),
        response_type = "dichotomous",
        estimate_type = "emmean"
      )
  )
  expect_snapshot(ard_emmeans_mean_estimates |> print(columns = "all"))

  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_estimates, stat_name %in% "method")[1],
    list(method = "Least-squares means")
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_estimates, stat_name %in% "estimate") |>
      unlist() |>
      unname(),
    glm(vs ~ am + mpg, data = mtcars, family = binomial) |>
      emmeans::emmeans(specs = ~am, regrid = "response") |>
      summary(infer = TRUE) |>
      getElement("prob")
  )

  # estimate_type = difference
  expect_silent(
    ard_emmeans_contrast_lme4 <-
      ard_emmeans_contrast(
        data = mtcars,
        formula = vs ~ am + (1 | cyl),
        method = "glmer",
        method.args = list(family = binomial),
        package = "lme4",
        response_type = "dichotomous"
      )
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_contrast_lme4, stat_name %in% "method")[1],
    list(method = "Least-squares mean difference")
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_contrast_lme4, stat_name %in% "estimate") |>
      unlist() |>
      unname(),
    lme4::glmer(vs ~ am + (1 | cyl), data = mtcars, family = binomial) |>
      emmeans::emmeans(specs = ~am, regrid = "response") |>
      emmeans::contrast(method = "pairwise") |>
      summary(infer = TRUE) |>
      getElement("estimate")
  )

  # estimate_type = emmean
  expect_silent(
    ard_emmeans_mean_estimates_lme4 <-
      ard_emmeans_contrast(
        data = mtcars,
        formula = vs ~ am + (1 | cyl),
        method = "glmer",
        method.args = list(family = binomial),
        package = "lme4",
        response_type = "dichotomous",
        estimate_type = "emmean"
      )
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_estimates_lme4, stat_name %in% "method")[1],
    list(method = "Least-squares means")
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_estimates_lme4, stat_name %in% "estimate") |>
      unlist() |>
      unname(),
    lme4::glmer(vs ~ am + (1 | cyl), data = mtcars, family = binomial) |>
      emmeans::emmeans(specs = ~am, regrid = "response") |>
      summary(infer = TRUE) |>
      getElement("prob")
  )

  # estimate_type = difference
  #styler: off
  expect_silent({
    data(api, package = "survey")
    ard_emmeans_contrast_svy <-
      survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
      ard_emmeans_contrast(
        formula = api00 ~ sch.wide,
        method = "svyglm",
        package = "survey"
      )}
  )
  # styler: on
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_contrast_svy, stat_name %in% "method")[1],
    list(method = "Least-squares mean difference")
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_contrast_svy, stat_name %in% "estimate") |>
      unlist() |>
      unname(),
    survey::svyglm(api00 ~ sch.wide, design = survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)) |>
      emmeans::emmeans(specs = ~sch.wide, regrid = "response") |>
      emmeans::contrast(method = "pairwise") |>
      summary(infer = TRUE) |>
      getElement("estimate")
  )

  # estimate_type = emmean
  #styler: off
  expect_silent({
    data(api, package = "survey")
    ard_emmeans_contrast_svy <-
      survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
      ard_emmeans_contrast(
        formula = api00 ~ sch.wide,
        method = "svyglm",
        package = "survey",
        estimate_type = "emmean"
      )}
  )
  # styler: on
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_contrast_svy, stat_name %in% "estimate") |>
      unlist() |>
      unname(),
    survey::svyglm(api00 ~ sch.wide, design = survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)) |>
      emmeans::emmeans(specs = ~sch.wide, regrid = "response") |>
      summary(infer = TRUE) |>
      getElement("emmean")
  )
})

test_that("ard_emmeans_contrast() follows ard structure", {
  expect_silent(
    ard_emmeans_contrast(
      data = mtcars,
      formula = vs ~ am + mpg,
      method = "glm",
      method.args = list(family = binomial),
      response_type = "dichotomous"
    ) |>
      cards::check_ard_structure()
  )
})

test_that("ard_emmeans_contrast() errors are returned correctly", {
  withr::local_options(width = 250)

  expect_silent(
    ard <- ard_emmeans_contrast(
      data = mtcars,
      formula = vs ~ am + mpg,
      method = "glm",
      method.args = list(family = nothing),
      response_type = "dichotomous"
    )
  )

  expect_snapshot(ard |> print(columns = "all"))

  expect_length(unique(ard$error), 1)
  expect_snapshot_value(ard$error[[1]])
})
