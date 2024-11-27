test_that("ard_event_rates() works with default settings", {
  withr::local_options(list(width = 200))

  expect_silent(
    res <- ard_event_rates(
      cards::ADAE,
      variables = AESOC,
      id = USUBJID,
      by = TRTA
    )
  )
  expect_snapshot(res |> print(n = 20, columns = "all"))

  expect_equal(
    res |>
      dplyr::filter(
        group1_level == "Placebo",
        variable_level == "CARDIAC DISORDERS",
        stat_name == "n"
      ) |>
      cards::get_ard_statistics(),
    list(
      n = cards::ADAE |>
        dplyr::filter(
          TRTA == "Placebo",
          AESOC == "CARDIAC DISORDERS"
        ) |>
        dplyr::slice_tail(n = 1L, by = all_of(c("USUBJID", "TRTA", "AESOC"))) |>
        nrow()
    )
  )

  # with denominator
  expect_snapshot(
    ard_event_rates(
      cards::ADAE |> dplyr::group_by(TRTA),
      variables = AESOC,
      id = USUBJID,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM)
    ) |>
      print(n = 20, columns = "all")
  )

  # with multiple variables
  expect_silent(
    res2 <- ard_event_rates(
      cards::ADAE,
      variables = c(SEX, AESOC),
      id = USUBJID,
      by = TRTA
    )
  )
  expect_equal(unique(res2$variable), c("SEX", "AESOC"))
  expect_equal(
    res,
    res2[-c(1:18), ]
  )
})

test_that("ard_event_rates(statistic) works", {
  withr::local_options(list(width = 200))

  expect_snapshot(
    ard_event_rates(
      cards::ADAE,
      variables = SEX,
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      statistic = ~"n"
    )
  )
})

test_that("ard_event_rates(ordered) works", {
  withr::local_options(list(width = 200))

  # pre-ordered factor variable
  adae <- cards::ADAE |>
    dplyr::mutate(AESEV = factor(cards::ADAE$AESEV, ordered = TRUE))

  expect_silent(
    res <- ard_event_rates(
      cards::ADAE,
      variables = AESEV,
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      ordered = TRUE
    )
  )
  expect_snapshot(res |> print(n = 20, columns = "all"))

  expect_equal(
    res |>
      dplyr::filter(
        group1_level == "Placebo",
        variable_level == "MODERATE",
        stat_name == "n"
      ) |>
      cards::get_ard_statistics(),
    list(
      n = adae |>
        dplyr::arrange(AESEV) |>
        dplyr::slice_tail(n = 1L, by = all_of(c("USUBJID", "TRTA"))) |>
        dplyr::filter(
          TRTA == "Placebo",
          AESEV == "MODERATE"
        ) |>
        nrow()
    )
  )

  res_unord <- ard_event_rates(
    cards::ADAE,
    variables = AESEV,
    id = USUBJID,
    by = TRTA,
    denominator = cards::ADSL |> dplyr::rename(TRTA = ARM)
  )
  expect_true(res$stat[[1]] != res_unord$stat[[1]])

  res2 <- ard_event_rates(
    adae,
    variables = AESEV,
    id = USUBJID,
    by = TRTA,
    denominator = cards::ADSL |> dplyr::rename(TRTA = ARM)
  )
  expect_equal(res, res2)

  # multiple variables
  expect_silent(
    res3 <- ard_event_rates(
      adae,
      variables = c(SEX, AESEV),
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      ordered = c(FALSE, TRUE)
    )
  )
  expect_equal(res, res3[-c(1:18), ])

  # named vector
  expect_silent(
    res4 <- ard_event_rates(
      adae,
      variables = c(SEX, AESEV),
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      ordered = c(AESEV = TRUE, SEX = FALSE)
    )
  )
  expect_equal(res3, res4)

  # error - length does not match
  expect_snapshot(
    ard_event_rates(
      adae,
      variables = c(SEX, AESEV),
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      ordered = TRUE
    ),
    error = TRUE
  )
  expect_equal(res, res2)
})

test_that("ard_event_rates() errors with incomplete factor columns", {
  # Check error when factors have no levels
  expect_snapshot(
    error = TRUE,
    ard_event_rates(
      cards::ADAE |>
        dplyr::mutate(AESOC = factor(AESOC, levels = character(0))),
      variables = AESOC,
      id = USUBJID,
      by = TRTA
    )
  )

  # Check error when factor has NA level
  expect_snapshot(
    error = TRUE,
    ard_event_rates(
      cards::ADAE |>
        dplyr::mutate(SEX = factor(SEX, levels = c("F", "M", NA), exclude = NULL)),
      variables = SEX,
      id = USUBJID,
      by = TRTA
    )
  )
})

test_that("ard_hierarchical_count() works with by variable not present in 'denominator'", {
  expect_silent(
    ard_events_with_by <- ard_event_rates(
      data = cards::ADAE,
      variables = AESOC,
      id = USUBJID,
      by = c(TRTA, AESEV),
      statistic = ~"n"
    )
  )

  expect_equal(
    ard_events_with_by |>
      dplyr::filter(
        group1_level == "Placebo",
        group2_level == "MILD",
        variable_level == "CARDIAC DISORDERS"
      ) |>
      cards::get_ard_statistics(),
    list(
      n = cards::ADAE |>
        dplyr::filter(
          TRTA == "Placebo",
          AESEV == "MILD",
          AESOC == "CARDIAC DISORDERS"
        ) |>
        dplyr::slice_tail(n = 1L, by = all_of(c("USUBJID", "TRTA", "AESEV", "AESOC"))) |>
        nrow()
    )
  )
})

test_that("ard_event_rates() works without any variables", {
  expect_snapshot(
    ard_event_rates(
      data = cards::ADAE,
      variables = starts_with("xxxx"),
      id = USUBJID,
      by = c(TRTA, AESEV)
    )
  )
})

test_that("ard_event_rates() follows ard structure", {
  expect_silent(
    ard_event_rates(
      cards::ADAE,
      variables = AESOC,
      id = USUBJID
    ) |>
      cards::check_ard_structure(method = FALSE)
  )
})
