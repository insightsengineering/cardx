pkg_name <- "cardx"
library(testthat)
test_check(pkg_name, reporter = ParallelProgressReporter$new())
