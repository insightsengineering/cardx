skip_if_not(cards::is_pkg_installed("survey", reference_pkg = "cardx"))

test_that("unstratified ard_survey_svycontinuous() works", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_error(
    ard_uni_svy_cont <-
      ard_survey_svycontinuous(
        dclus1,
        variables = api00,
        statistic = ~ c(
          "mean", "median", "min", "max", "sum", "var", "sd",
          "mean.std.error", "deff", "p75"
        )
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_uni_svy_cont))

  # check the calculated stats are correct
  expect_equal(
    cards::get_ard_statistics(ard_uni_svy_cont, stat_name %in% "mean") |> unlist(),
    survey::svymean(x = ~api00, dclus1, na.rm = TRUE)[1] |> unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_uni_svy_cont, stat_name %in% "median") |> unlist(),
    survey::svyquantile(x = ~api00, dclus1, na.rm = TRUE, quantiles = 0.5)[[1]][1] |> unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_uni_svy_cont, stat_name %in% "min") |> unlist(),
    dclus1$variables$api00 |> min(na.rm = TRUE),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_uni_svy_cont, stat_name %in% "max") |> unlist(),
    dclus1$variables$api00 |> max(na.rm = TRUE),
    ignore_attr = TRUE
  )
  expect_equal(
    cards::get_ard_statistics(ard_uni_svy_cont, stat_name %in% "var") |> unlist(),
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

  expect_snapshot(ard_uni_svy_cont)
})


test_that("stratified ard_survey_svycontinuous() works", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_error(
    ard_svy_cont <-
      ard_survey_svycontinuous(
        dclus1,
        by = both,
        variables = api00,
        statistic = ~ c(
          "mean", "median", "min", "max", "sum", "var", "sd",
          "mean.std.error", "deff", "p75"
        )
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cont))

  # check the calculated stats are correct
  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "mean") %>%
      {
        dplyr::pull(., stat) |> set_names(unlist(.$group1_level))
      },
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svymean, na.rm = TRUE) %>%
      {
        dplyr::pull(., api00) |>
          as.list() |>
          set_names(rownames(.))
      },
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "median") %>%
      {
        dplyr::pull(., stat) |> set_names(unlist(.$group1_level))
      },
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svyquantile, na.rm = TRUE, quantiles = 0.5) %>%
      {
        dplyr::pull(., api00) |>
          as.list() |>
          set_names(rownames(.))
      },
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "min") %>%
      {
        dplyr::pull(., stat) |> set_names(unlist(.$group1_level))
      },
    dclus1$variables |>
      dplyr::summarise(
        .by = both,
        min = min(api00, na.rm = TRUE)
      ) |>
      dplyr::arrange(both) %>%
      {
        dplyr::pull(., min) |>
          as.list() |>
          set_names(.$both)
      },
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "max") %>%
      {
        dplyr::pull(., stat) |> set_names(unlist(.$group1_level))
      },
    dclus1$variables |>
      dplyr::summarise(
        .by = both,
        max = max(api00, na.rm = TRUE)
      ) |>
      dplyr::arrange(both) %>%
      {
        dplyr::pull(., max) |>
          as.list() |>
          set_names(.$both)
      },
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "var") %>%
      {
        dplyr::pull(., stat) |> set_names(unlist(.$group1_level))
      },
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svyvar, na.rm = TRUE) %>%
      {
        dplyr::pull(., api00) |>
          as.list() |>
          set_names(rownames(.))
      },
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "sd") %>%
      {
        dplyr::pull(., stat) |> set_names(unlist(.$group1_level))
      },
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svyvar, na.rm = TRUE) %>%
      {
        dplyr::pull(., api00) |>
          sqrt() |>
          as.list() |>
          set_names(rownames(.))
      },
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "mean.std.error") %>%
      {
        dplyr::pull(., stat) |> set_names(unlist(.$group1_level))
      },
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svymean, na.rm = TRUE) %>%
      {
        dplyr::pull(., se) |>
          as.list() |>
          set_names(rownames(.))
      },
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "deff") %>%
      {
        dplyr::pull(., stat) |> set_names(unlist(.$group1_level))
      },
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svymean, na.rm = TRUE, deff = TRUE) %>%
      {
        dplyr::pull(., DEff.api00) |>
          as.list() |>
          set_names(rownames(.))
      },
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "p75") %>%
      {
        dplyr::pull(., stat) |> set_names(unlist(.$group1_level))
      },
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svyquantile, na.rm = TRUE, quantiles = 0.75) %>%
      {
        dplyr::pull(., api00) |>
          as.list() |>
          set_names(rownames(.))
      },
    ignore_attr = TRUE
  )
})

test_that("ard_survey_svycontinuous() NA handling", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1 |> dplyr::mutate(api00 = NA_real_), fpc = ~fpc)

  expect_error(
    ard_uni_NA_svy_cont <-
      ard_survey_svycontinuous(
        dclus1,
        variables = api00,
        statistic = ~ c(
          "mean", "median", "min", "max", "sum", "var", "sd",
          "mean.std.error", "deff", "p75"
        )
      ),
    NA
  )
  # all results are NA, NaN, or NULL
  expect_true(
    ard_uni_NA_svy_cont$stat |>
      map_lgl(~ is.na(.x) || is.nan(.x) || is.null(.x)) |>
      all()
  )

  expect_error(
    ard_NA_svy_cont <-
      ard_survey_svycontinuous(
        dclus1,
        variables = api00,
        by = both,
        statistic = ~ c(
          "mean", "median", "min", "max", "sum", "var", "sd",
          "mean.std.error", "deff", "p75"
        )
      ),
    NA
  )
  # all results are NA, NaN, or NULL
  expect_true(
    ard_NA_svy_cont$stat |>
      map_lgl(~ is.na(.x) || is.nan(.x) || is.null(.x)) |>
      all()
  )
})

test_that("ard_survey_svycontinuous() error handling", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1[1:20, ], fpc = ~fpc)

  # passing a character vector (some results are still calculable...i don't think we need to "fix" these)
  # and these "results" may vary across systems (all are nonsense), so just check
  # that code runs without error
  expect_error(
    ard_survey_svycontinuous(
      dclus1,
      variables = sname,
      statistic = ~ c(
        "mean", "median", "min", "max", "sum", "var", "sd",
        "mean.std.error", "deff", "p75"
      )
    ),
    NA
  )

  expect_error(
    ard_survey_svycontinuous(
      dclus1,
      variables = sname,
      by = both,
      statistic = ~ c(
        "mean", "median", "min", "max", "sum", "var", "sd",
        "mean.std.error", "deff", "p75"
      )
    ),
    NA
  )
})

test_that("ard_survey_svycontinuous(fmt_fn)", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_snapshot(
    ard_survey_svycontinuous(
      dclus1,
      variables = api00,
      statistic = ~ c("mean", "median", "min", "max"),
      fmt_fn = list(api00 = list(mean = 2, median = "xx.xx", min = as.character))
    )
  )
})

test_that("ard_survey_svycontinuous(stat_label)", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_snapshot(
    ard_survey_svycontinuous(
      dclus1,
      variables = api00,
      statistic = ~ c("mean", "median", "min", "max"),
      stat_label = list(api00 = list(mean = "MeAn", median = "MEDian", min = "MINimum"))
    )
  )
})

test_that("ard_survey_svycontinuous(by) unobserved levels/combinations", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(
    id = ~dnum, weights = ~pw,
    data = apiclus1 |>
      dplyr::mutate(
        both = factor(both, levels = c("Yes", "No", "Neither")),
        awards = ifelse(stype == "E", "Yes", as.character(awards))
      ),
    fpc = ~fpc
  )


  # The 'Neither' level is never observed, but included in the table
  expect_setequal(
    levels(dclus1$variables$both),
    ard_survey_svycontinuous(
      dclus1,
      variables = api00,
      by = both,
      statistic = ~ c("mean", "median", "min", "max")
    ) |>
      dplyr::pull(group1_level) |>
      map_chr(as.character) |>
      unique()
  )

  # stype="E" is not observed with awards="No", but it should still appear in table
  with(dclus1$variables, table(stype, awards))
  expect_equal(
    ard_survey_svycontinuous(
      dclus1,
      variables = api00,
      by = c(stype, awards),
      statistic = ~ c("mean", "median", "min", "max")
    ) |>
      dplyr::filter(map_chr(group1_level, as.character) %in% "E", group2_level %in% "No") |>
      dplyr::pull(stat),
    rep_len(list(NA_real_), 4L)
  )
})
