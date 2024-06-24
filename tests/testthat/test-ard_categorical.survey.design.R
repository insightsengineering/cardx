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
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_col, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # check the calculated stats are correct

  # section 1: by variable, row denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p_unweighted") |> unlist()
  )

  # section 2: by variable, column denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p_unweighted") |> unlist()
  )

  # section 3: by variable, cell denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p_unweighted") |> unlist()
  )


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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # check the calculated stats are correct

  # section 4: without by variable, row denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p_unweighted") |> unlist()
  )

  # section 5: without by variable, column denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p_unweighted") |> unlist()
  )

  # section 6: without by variable, cell denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p_unweighted") |> unlist(),
    cards::ard_categorical(svy_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p_unweighted") |> unlist()
  )


})

# - What happens with a variable that is all NA? How does that behavior compare to `ard_categorical()` for data frames ----
# Issues with NA level in general, will reinstate these tests once they've been resolved.
test_that("ard_categorical.survey.design() works when variables have all NAs", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

  # row denom
  svy_titanic$variables$Class <- NA
  svy_titanic$variables$Class <- fct_na_value_to_level(svy_titanic$variables$Class)

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
  )

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

  # cell denom
  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      )
  )
})

# - Do we get results for unobserved factor levels in the `by` and `variable` variables?
test_that("ard_categorical.survey.design() works for unobserved factor levels", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Survived <- fct_expand(svy_titanic$variables$Survived, "Unknown")

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_col, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variables have unobserved levels, no by variable
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Class <- fct_expand(svy_titanic$variables$Survived, "Peasant")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variable AND by have unobserved levels
  svy_titanic$variables$Survived <- fct_expand(svy_titanic$variables$Survived, "Unknown")

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))
})

# - Do we get results for unobserved logical levels in the `by` and `variable` variables?
test_that("ard_categorical.survey.design() works for unobserved logical levels", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Survived <- rep(TRUE, length(svy_titanic$variables$Survived))
  svy_titanic$variables$Survived <- as.logical(fct_expand(svy_titanic$variables$Survived, FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_col, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variables have unobserved levels, no by variable
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Age <- rep(TRUE, length(svy_titanic$variables$Age))

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variable AND by have unobserved levels
  svy_titanic$variables$Survived <- rep(TRUE, length(svy_titanic$variables$Survived))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))
})

# - Does the work around apply for variables with only 1 level
test_that("ard_categorical.survey.design() works with variables with only 1 level", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Survived <- rep("Yes", length(svy_titanic$variables$Survived))

  # by variable only has 1 level
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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_col, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variables have only 1 level, no by variable
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Age <- as.factor(rep("Child", length(svy_titanic$variables$Age)))

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variable AND by have only 1 level
  svy_titanic$variables$Survived <- as.factor(rep("Yes", length(svy_titanic$variables$Survived)))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

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
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))
})
