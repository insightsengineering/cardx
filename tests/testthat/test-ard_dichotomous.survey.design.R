# Test survey.design works
test_that("ard_dichotomous.survey.design() works", {
  svy_dicho <- survey::svydesign(ids = ~1, data = mtcars, weights = ~1)

  # row denom with by var
  expect_error(
    ard_dichotomous_row <-
      ard_dichotomous(svy_dicho,
                      by = vs,
                      variables = c(cyl, am),
                      value = list(cyl = 4),
                      denominator = "row"),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_row, method = FALSE))

  # col denom with by var
  expect_error(
    ard_dichotomous_col <- ard_dichotomous(svy_dicho, by = vs,
                                           variables = c(cyl, am),
                                           value = list(cyl = 4),
                                           denominator = "column"),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_col, method = FALSE))

  # col denom with by var
  expect_error(
    ard_dichotomous_cell <- ard_dichotomous(svy_dicho, by = vs,
                                           variables = c(cyl, am),
                                           value = list(cyl = 4),
                                           denominator = "cell"),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_cell, method = FALSE))


  # test individual stats

  # section 1: by variable, row denominator
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "n") |> unlist(),
    cards::ard_dichotomous(mtcars,
                           by = vs,
                           variables = c(cyl, am),
                           value = list(cyl = 4),
                           denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "N") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "p") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "deff") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "deff") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  # section 2: by variable, column denominator
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "n") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "N") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "p") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "deff") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "deff") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  # section 3: by variable, cell denominator
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "n") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "N") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "p") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "deff") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "deff") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )


  # row denom without by var
  expect_error(
    ard_dichotomous_row <- ard_dichotomous(svy_dicho, by = NULL,
                                           variables = c(cyl, am),
                                           value = list(cyl = 4),
                                           denominator = "row"),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_row, method = FALSE))

  # col denom without by var
  expect_error(
    ard_dichotomous_col <- ard_dichotomous(svy_dicho, by = NULL,
                                           variables = c(cyl, am),
                                           value = list(cyl = 4),
                                           denominator = "column"),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_col, method = FALSE))

  # col denom without by var
  expect_error(
    ard_dichotomous_cell <- ard_dichotomous(svy_dicho, by = NULL,
                                            variables = c(cyl, am),
                                            value = list(cyl = 4),
                                            denominator = "cell"),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_cell, method = FALSE))


  # test individual stats

  # section 1: by variable, row denominator
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "n") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "N") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "p") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "deff") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "deff") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  # section 2: by variable, column denominator
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "n") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "N") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "p") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "deff") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "deff") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )

  # section 3: by variable, cell denominator
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "n") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "N") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "p") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "p.std.error") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p.std.error") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "deff") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "deff") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "n_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n_unweighted") |> unlist()
  )
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "N_unweighted") |> unlist(),
    cards::ard_dichotomous(svy_dicho, by = NULL, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N_unweighted") |> unlist()
  )
})



