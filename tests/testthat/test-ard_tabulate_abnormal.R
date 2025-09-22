set.seed(1)
adlb <- cards::ADLB
adlb$BNRIND <- ifelse(adlb$BNRIND != "N", sample(c("LOW", "HIGH"), nrow(adlb), replace = TRUE), "NORMAL")

test_that("ard_tabulate_abnormal() works", {
  withr::local_options(list(width = 200))

  # default abnormal
  expect_message(
    expect_message(
      res <- adlb |>
        ard_tabulate_abnormal(postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA)
    )
  )
  expect_snapshot(res |> print(columns = "all"))

  # custom abnormal, no `by`
  expect_snapshot(
    adlb |>
      ard_tabulate_abnormal(
        postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID,
        abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH"), other = "OTHER")
      ) |>
      print(columns = "all")
  )

  # excl_baseline_abn=FALSE
  expect_snapshot(
    adlb |>
      ard_tabulate_abnormal(postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA, excl_baseline_abn = FALSE) |>
      print(columns = "all")
  )

  # quiet=TRUE
  expect_silent(
    res <- adlb |>
      ard_tabulate_abnormal(postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA, quiet = TRUE)
  )
})

test_that("ard_tabulate_abnormal() errors are handled correctly", {
  # unnamed abnormal
  expect_snapshot(
    res <- adlb |>
      ard_tabulate_abnormal(
        postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA, abnormal = list("HIGH", "LOW")
      ),
    error = TRUE
  )

  # incorrect abnormality type
  expect_snapshot(
    res <- adlb |>
      ard_tabulate_abnormal(
        postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA, abnormal = list(high = 1:5, low = 0)
      ),
    error = TRUE
  )
})
