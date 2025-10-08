skip_if_pkg_not_installed("survey")

test_that("ard_total_n.survey.design() works", {
  expect_snapshot(
    survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
      ard_total_n()
  )
})

test_that("ard_total_n.survey.design() follows ard structure", {
  expect_silent(
    survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
      ard_total_n() |>
      cards::check_ard_structure(method = FALSE)
  )
})

# test for https://stackoverflow.com/questions/79673577
test_that("ard_total_n.survey.design() using `update()`", {
  database <- data.frame(
    INDIV_AGE = rnorm(100, mean = 50, sd = 4),
    INDIV_GENDER = rbinom(n = 100, size = 1, prob = 0.6),
    PAIN_SCALE = factor(sample(c("Low", "Elevated"), size = 100, replace = T)),
    FLOWER_COLOR = factor(sample(c("Blue", "Red"), size = 100, replace = T)),
    poids = rnorm(100, mean = 2, sd = 0.8)
  )
  database[1, "INDIV_GENDER"] <- NA

  expect_silent(
    survey::svydesign(
      id = ~1,
      weights = ~poids,
      data = database
    ) |>
      subset(!is.na(INDIV_GENDER)) |>
      ard_total_n()
  )
})
