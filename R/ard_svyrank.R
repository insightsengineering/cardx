.compute_svyrank <- function(data, variable, by = NULL, test_name) {
  # difftime variable needs to be transformed into numeric for svyquantile
  if (inherits(data$variables[[variable]], "difftime")) {
    data$variables[[variable]] <- unclass(data$variables[[variable]])
  }

  # styler: off
  if (test_name %in% "mean") args <- list(FUN = survey::svymean)
  else if (test_name %in% "sum") args <- list(FUN = survey::svytotal)
  else if (test_name %in% "var") args <- list(FUN = survey::svyvar)

}
