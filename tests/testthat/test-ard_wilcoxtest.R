test_that("ard_wilcoxtest() works", {
  expect_error(
    ard_wilcoxtest <-
      cards::ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_wilcoxtest(by = ARM, variable = AGE, correct = FALSE, conf.int = TRUE),
    NA
  )

  expect_equal(
    ard_wilcoxtest |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    wilcox.test(
      AGE ~ ARM,
      data = cards::ADSL |> dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")),
      conf.int = TRUE,
      correct = FALSE
    ) |>
      broom::tidy() |>
      dplyr::select(estimate, conf.low, conf.high) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_equal(
    cards::ADSL |>
      ard_wilcoxtest(by = ARM, variable = AGE, correct = FALSE) |>
      dplyr::pull(error) |>
      getElement(1L),
    "grouping factor must have exactly 2 levels"
  )
})

test_that("ard_paired_wilcoxtest() works", {
  ADSL_paired <-
    cards::ADSL[c("ARM", "AGE")] |>
    dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
    dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number())

  expect_error(
    ard_paired_wilcoxtest <-
      ADSL_paired |>
      ard_paired_wilcoxtest(
        by = ARM, variable = AGE, id = USUBJID,
        correct = FALSE, conf.int = TRUE
      ),
    NA
  )

  expect_equal(
    ard_paired_wilcoxtest |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    with(
      data =
        dplyr::full_join(
          ADSL_paired |> dplyr::filter(ARM %in% "Placebo") |> dplyr::rename(ARM1 = ARM, AGE1 = AGE),
          ADSL_paired |> dplyr::filter(ARM %in% "Xanomeline High Dose") |> dplyr::rename(ARM2 = ARM, AGE2 = AGE),
          by = "USUBJID"
        ),
      expr =
        wilcox.test(
          x = AGE1,
          y = AGE2,
          paired = TRUE,
          correct = FALSE,
          conf.int = TRUE
        ) |>
          broom::tidy() |>
          dplyr::select(estimate, conf.low, conf.high) |>
          unclass()
    ),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_snapshot(
    ADSL_paired |>
      dplyr::mutate(
        ARM = ifelse(dplyr::row_number() == 1L, "3rd ARM", ARM)
      ) |>
      ard_paired_wilcoxtest(
        by = ARM, variable = AGE, id = USUBJID,
        correct = FALSE, conf.int = TRUE
      ) |>
      as.data.frame()
  )
})
