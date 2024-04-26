---
editor_options: 
  markdown: 
    wrap: 72
---

# cardx <a href="https://insightsengineering.github.io/cardx"><img src="man/figures/logo.png" alt="cardx website" align="right" height="138"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/insightsengineering/cardx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/insightsengineering/cardx/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/insightsengineering/cardx/branch/main/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/cardx?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

This is the source repository of the `cardx` R package.

The {cardx} package is an extension of the {cards} package, providing
additional functions to create Analysis Results Data Objects (ARDs)
using the **R** programming language. The {cardx} package exports ARD
functions that uses utility functions from {cards} and statistical
functions from additional packages (such as {stats}, {aod}, {car},
{survey}, etc.) to construct summary objects.

Summary objects can be used to:

-   [**Generate Tables and visualizations for Regulatory Submission**]
    easily in **R**. Perfect for presenting descriptive statistics,
    statistical analyses, regressions, etc. and more.

-   [**Conduct Quality Control checks on existing Tables** ] in R.
    Storing both the results and test parameters supports the re-use and
    verification of data analyses.

In the future, components of the {cards} package will be integrated
seamlessly with the {gtsummary} package for table creation. ARDs will be
a byproduct of {gtsummary} tables and ARDs will be acceptable input for
{gtsummary} functions as detailed in the image below:

<img src="man/figures/README-ard-gtsummary-workflow.png" width="75%"/>

## Installation

The latest development version of `{cardx}` can directly be installed
from GitHub by running the following:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("insightsengineering/cardx")
```

## Examples

### Summary Table

Use
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
to summarize a data frame.

Example ttest:

``` r
library(cardx)

# summarize the data with our package
cards::ADSL |> 
  # keep two treatment arms for the t-test calculation
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |> 
  cardx::ard_stats_t_test(by = ARM, variable = AGE)
```

<img src="man/figures/README-t_test_print_simple.png" width="75%"/>
