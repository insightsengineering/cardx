set.seed(1)
adlb <- cards::ADLB
adlb$BNRIND <- ifelse(adlb$BNRIND != "N", sample(c("LOW", "HIGH"), nrow(adlb), replace = TRUE), "NORMAL")

test_that("ard_abnormal() works", {
  withr::local_options(list(width = 200))

  # default abnormal
  expect_silent(
    res <- adlb |>
      ard_abnormal(postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA)
  )
  expect_snapshot(res |> print(columns = "all"))

  # custom abnormal, no `by`
  expect_silent(
    res <- adlb |>
      ard_abnormal(
        postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID,
        abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH"), other = "OTHER")
      )
  )
  expect_snapshot(res |> print(columns = "all"))

  # excl_baseline_abn=FALSE
  expect_silent(
    res <- adlb |>
      ard_abnormal(postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA, excl_baseline_abn = FALSE)
  )
  expect_snapshot(res |> print(columns = "all"))
})

test_that("ard_abnormal() errors are handled correctly", {
  # unnamed abnormal
  expect_snapshot(
    res <- adlb |>
      ard_abnormal(
        postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA, abnormal = list("HIGH", "LOW")
      ),
    error = TRUE
  )

  # incorrect abnormality type
  expect_snapshot(
    res <- adlb |>
      ard_abnormal(
        postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA, abnormal = list(high = 1:5, low = 0)
      ),
    error = TRUE
  )
})
