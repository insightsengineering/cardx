skip_if_not(cards::is_pkg_installed(c("survival", "broom"), reference_pkg = "cardx"))

test_that("ard_survfit() works with times provided", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(times = c(60, 180)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survfit() works with different type", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(times = c(60, 180), type = "risk") |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survfit() works with probs provided", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(probs = c(0.25, 0.75), type = "cumhaz") |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survfit() works with competing risks", {
  set.seed(1)
  ADTTE_MS <- cards::ADTTE %>%
    dplyr::mutate(
      CNSR = dplyr::case_when(
        CNSR == 0 ~ "censor",
        runif(dplyr::n()) < 0.5 ~ "death from cancer",
        TRUE ~ "death other causes"
      ) %>% factor()
    )

  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, data = ADTTE_MS) %>%
      ard_survfit(times = c(60, 180)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survfit() errors are properly handled", {
  expect_snapshot(
    ard_survfit("not_survfit"),
    error = TRUE
  )

  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(times = 100, type = "notatype"),
    error = TRUE
  )

  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survfit(times = 100, probs = c(0.25, 0.75)),
    error = TRUE
  )
})
