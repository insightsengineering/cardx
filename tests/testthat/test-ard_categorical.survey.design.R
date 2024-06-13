# Items to test
# - First, everything needs to be tested independently for denominator='column'|'row'|'cell' AND by whether there is a by variable
#    This is because these 6 scenarios are calculated entirely separately
# - What happens with a variable that is all NA? How does that behavior compare to `ard_categorical()` for data frames
#    The function _should_ work if the underlying type is factor or logical
# - Do we get results for unobserved factor levels in the `by` and `variable` variables?
# - Do we get results for unobserved logical levels in the `by` and `variable` variables, e.g. if there are only TRUE, we should have FALSE rows too?
# - A trick to test survey data is to take a normal data frame, convert it to survey using equal weights. Then all the results should equal the unweighted summariess
#    dplyr::tibble(y = rep(FALSE, 15), x = rep(TRUE, 15)) |>
#      survey::svydesign(ids = ~1, data = _, weights = ~1) |>
#      ard_categorical(by = y)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
