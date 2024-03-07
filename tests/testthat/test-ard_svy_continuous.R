skip_if_not(cards::is_pkg_installed("survey", reference_pkg = "cardx"))

test_that("unstratified ard_svy_continuous() works", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_error(
    ard_uni_svy_cont <-
      ard_svy_continuous(
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


test_that("stratified ard_svy_continuous() works", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_error(
    ard_svy_cont <-
      ard_svy_continuous(
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
      {dplyr::pull(., stat) |> set_names(unlist(.$group1_level))},
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svymean, na.rm = TRUE) %>%
      {dplyr::pull(., api00) |> as.list() |> set_names(rownames(.))},
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "median") %>%
      {dplyr::pull(., stat) |> set_names(unlist(.$group1_level))},
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svyquantile, na.rm = TRUE, quantiles = 0.5) %>%
      {dplyr::pull(., api00) |> as.list() |> set_names(rownames(.))},
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "min") %>%
      {dplyr::pull(., stat) |> set_names(unlist(.$group1_level))},
    dclus1$variables |>
      dplyr::summarise(
        .by = both,
        min = min(api00, na.rm = TRUE)
      ) |>
      dplyr::arrange(both) %>%
      {dplyr::pull(., min) |> as.list() |> set_names(.$both)},
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "max") %>%
      {dplyr::pull(., stat) |> set_names(unlist(.$group1_level))},
    dclus1$variables |>
      dplyr::summarise(
        .by = both,
        max = max(api00, na.rm = TRUE)
      ) |>
      dplyr::arrange(both) %>%
      {dplyr::pull(., max) |> as.list() |> set_names(.$both)},
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "var") %>%
      {dplyr::pull(., stat) |> set_names(unlist(.$group1_level))},
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svyvar, na.rm = TRUE) %>%
      {dplyr::pull(., api00) |> as.list() |> set_names(rownames(.))},
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "sd") %>%
      {dplyr::pull(., stat) |> set_names(unlist(.$group1_level))},
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svyvar, na.rm = TRUE) %>%
      {dplyr::pull(., api00) |> sqrt() |> as.list() |> set_names(rownames(.))},
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "mean.std.error") %>%
      {dplyr::pull(., stat) |> set_names(unlist(.$group1_level))},
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svymean, na.rm = TRUE) %>%
      {dplyr::pull(., se) |> as.list() |> set_names(rownames(.))},
    ignore_attr = TRUE
  )

  expect_equal(
    ard_svy_cont |>
      dplyr::filter(stat_name %in% "deff") %>%
      {dplyr::pull(., stat) |> set_names(unlist(.$group1_level))},
    survey::svyby(~api00, by = ~both, dclus1, FUN = survey::svymean, na.rm = TRUE, deff = TRUE) %>%
      {dplyr::pull(., DEff.api00) |> as.list() |> set_names(rownames(.))},
    ignore_attr = TRUE
  )


  # expect_equal(
  #   cards::get_ard_statistics(ard_svy_cont, stat_name %in% "deff") |> unlist(),
  #   survey::svymean(x = ~api00, dclus1, na.rm = TRUE, deff = TRUE) |>
  #     as.data.frame() |>
  #     dplyr::pull(deff),
  #   ignore_attr = TRUE
  # )
  # expect_equal(
  #   cards::get_ard_statistics(ard_svy_cont, stat_name %in% "p75") |> unlist(),
  #   survey::svyquantile(x = ~api00, dclus1, na.rm = TRUE, quantiles = 0.75)[[1]][1] |> unlist(),
  #   ignore_attr = TRUE
  # )
})
