skip_if_not(is_pkg_installed(c("effectsize", "parameters")))

test_that("ard_effectsize_hedges_g() works", {
  expect_error(
    ard_hedges_g <-
      cards::ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_effectsize_hedges_g(by = ARM, variable = AGE),
    NA
  )

  expect_equal(
    ard_hedges_g |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    effectsize::hedges_g(
      AGE ~ ARM,
      data = cards::ADSL |> dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose"))
    ) |>
      parameters::standardize_names(style = "broom") |>
      dplyr::select(estimate, conf.low, conf.high),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_equal(
    cards::ADSL |>
      ard_effectsize_hedges_g(by = ARM, variable = AGE) |>
      dplyr::select(error) %>%
      is.null(),
    FALSE
  )

  # test that the function works with multiple variables as once
  expect_snapshot(
    cards::ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_effectsize_hedges_g(by = ARM, variables = c(BMIBL, HEIGHTBL)) |>
      dplyr::select(c(1:3, 5:6)) |>
      dplyr::group_by(variable) |>
      dplyr::slice_head(n = 3) |>
      as.data.frame()
  )
})

test_that("ard_effectsize_paired_hedges_g() works", {
  ADSL_paired <-
    cards::ADSL[c("ARM", "AGE")] |>
    dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
    dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
    dplyr::group_by(USUBJID) |>
    dplyr::filter(dplyr::n() > 1)

  expect_error(
    ard_paired_hedges_g <-
      ADSL_paired |>
      ard_effectsize_paired_hedges_g(by = ARM, variable = AGE, id = USUBJID),
    NA
  )

  expect_equal(
    ard_paired_hedges_g |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    with(
      data =
        dplyr::full_join(
          ADSL_paired |> dplyr::filter(ARM %in% "Placebo") |> dplyr::rename(ARM1 = ARM, AGE1 = AGE),
          ADSL_paired |> dplyr::filter(ARM %in% "Xanomeline High Dose") |> dplyr::rename(ARM2 = ARM, AGE2 = AGE),
          by = "USUBJID"
        ),
      expr =
        effectsize::hedges_g(
          x = AGE1,
          y = AGE2,
          paired = TRUE
        ) |>
          parameters::standardize_names(style = "broom") |>
          dplyr::select(estimate, conf.low, conf.high)
    ),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_equal(
    ADSL_paired |>
      dplyr::mutate(
        ARM = ifelse(dplyr::row_number() == 1L, "3rd ARM", ARM)
      ) |>
      ard_effectsize_paired_hedges_g(by = ARM, variable = AGE, id = USUBJID) |>
      dplyr::select(error) %>%
      is.null(),
    FALSE
  )
})

test_that("ard_effectsize_cohens_d() follows ard structure", {
  expect_silent(
    cards::ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_effectsize_hedges_g(by = ARM, variables = AGE, pooled_sd = FALSE) |>
      cards::check_ard_structure(method = FALSE)
  )

  # paired
  ADSL_paired <-
    cards::ADSL[c("ARM", "AGE")] |>
    dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
    dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
    dplyr::group_by(USUBJID) |>
    dplyr::filter(dplyr::n() > 1)

  expect_silent(
    ADSL_paired |>
      ard_effectsize_paired_hedges_g(by = ARM, variable = AGE, id = USUBJID) |>
      cards::check_ard_structure(method = FALSE)
  )
})
