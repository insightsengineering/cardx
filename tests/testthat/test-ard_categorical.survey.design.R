# Items to test
# - First, everything needs to be tested independently for denominator='column'|'row'|'cell' AND by whether there is a by variable
#    This is because these 6 scenarios are calculated entirely separately
# - What happens with a variable that is all NA? How does that behavior compare to `ard_categorical()` for data frames
#    The function _should_ work if the underlying type is factor or logical
# - Do we get results for unobserved factor levels in the `by` and `variable` variables?
# - Do we get results for unobserved logical levels in the `by` and `variable` variables, e.g. if there are only TRUE, we should have FALSE rows too?
# - It turns out variables (both by and variables) that only have one level are problematic in some ways.
#     I've coded around these issues, but we need thorough testing when either by or a variable has a single level.
#     We need tests for when these variables are factor, logical, and other to ensure every case is handled properly.
# - A trick to test survey data is to take a normal data frame, convert it to survey using equal weights.
#     Then all the results should equal the unweighted summaries and we can perform expect_equal() checks against the data.frame S3 methods.
#    dplyr::tibble(y = rep(FALSE, 15), x = rep(TRUE, 15)) |>
#      survey::svydesign(ids = ~1, data = _, weights = ~1) |>
#      ard_categorical(by = y)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Test survey.design working (2x3)
test_that("ard_categorical.survey.design() works", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

  # denom = row, with by
  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row))

  # denom = column, with by
  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  # denom = cell, with by
  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))


  # denom = row, without by
  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row))

  # denom = column, without by
  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  # denom = cell, without by
  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))


  # check the calculated stats are correct
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n") |> unlist(),
    survey::svymean(x = ~api00, dclus1, na.rm = TRUE)[1] |> unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N") |> unlist(),
    survey::svyquantile(x = ~api00, dclus1, na.rm = TRUE, quantiles = 0.5)[[1]][1] |> unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p") |> unlist(),
    dclus1$variables$api00 |> min(na.rm = TRUE),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "deff") |> unlist(),
    dclus1$variables$api00 |> max(na.rm = TRUE),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p.std.error") |> unlist(),
    survey::svyvar(x = ~api00, dclus1, na.rm = TRUE)[1] |> unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_uni_svy_cont, stat_name %in% "sd") |> unlist(),
    survey::svyvar(x = ~api00, dclus1, na.rm = TRUE)[1] |> unlist() |> sqrt(),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_uni_svy_cont, stat_name %in% "mean.std.error") |> unlist(),
    survey::svymean(x = ~api00, dclus1, na.rm = TRUE) |> survey::SE() |> unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_uni_svy_cont, stat_name %in% "deff") |> unlist(),
    survey::svymean(x = ~api00, dclus1, na.rm = TRUE, deff = TRUE) |>
      as.data.frame() |>
      dplyr::pull(deff),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_uni_svy_cont, stat_name %in% "p75") |> unlist(),
    survey::svyquantile(x = ~api00, dclus1, na.rm = TRUE, quantiles = 0.75)[[1]][1] |> unlist(),
    ignore_attr = TRUE
  )

})

# - What happens with a variable that is all NA? How does that behavior compare to `ard_categorical()` for data frames
test_that("ard_categorical.survey.design() works when variables have all NAs", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

  #row denom
  svy_titanic$variables$Class <- NA
  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row))

  # column denom
  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  # cell denom
  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

})

# - Do we get results for unobserved factor levels in the `by` and `variable` variables?
test_that("ard_categorical.survey.design() works for unobserved factor levels", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Survived <- standalone:::fct_expand(svy_titanic$variables$Survived, "Unknown")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  # variables have unobserved levels, no by variable
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Class <- standalone:::fct_expand(svy_titanic$variables$Survived, "Peasant")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  # variable AND by have unobserved levels
  svy_titanic$variables$Survived <- standalone:::fct_expand(svy_titanic$variables$Survived, "Unknown")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

})

# - Do we get results for unobserved logical levels in the `by` and `variable` variables?
test_that("ard_categorical.survey.design() works for unobserved factor levels", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Survived <- ifelse(svy_titanic$variables$Survived == "Yes", TRUE, FALSE)
  svy_titanic$variables$Survived <- standalone:::fct_expand(svy_titanic$variables$Survived, "Unknown")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  # variables have unobserved levels, no by variable
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Age <- ifelse(svy_titanic$variables$Age == "Child", TRUE, FALSE)
  svy_titanic$variables$Age <- standalone:::fct_expand(svy_titanic$variables$Survived, "Unknown")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  # variable AND by have unobserved levels
  svy_titanic$variables$Survived <- ifelse(svy_titanic$variables$Survived == "Yes", TRUE, FALSE)
  svy_titanic$variables$Survived <- standalone:::fct_expand(svy_titanic$variables$Survived, "Unknown")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell))

})
