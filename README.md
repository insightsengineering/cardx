
# cardx <a href="https://insightsengineering.github.io/cardx/"><img src="man/figures/logo.png" alt="cardx website" align="right" height="138"/></a>

[![R-CMD-check](https://github.com/insightsengineering/cardx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/insightsengineering/cardx/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/insightsengineering/cardx/branch/main/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/cardx?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

The **{cardx}** package is an extension of the {cards} package,
providing additional functions to create Analysis Results Data Objects
(ARDs) using the **R** programming language. The {cardx} package exports
ARD functions that uses utility functions from {cards} and statistical
functions from additional packages (such as, {stats}, {mmrm}, {emmeans},
{car}, {survey}, etc.) to construct summary objects.

Summary objects can be used to:

- **Generate Tables and visualizations for Regulatory Submission**
  easily in **R**. Perfect for presenting descriptive statistics,
  statistical analyses, regressions, etc. and more.

- **Conduct Quality Control checks on existing Tables** in R. Storing
  both the results and test parameters supports the re-use and
  verification of data analyses.

## Installation

Install cards from CRAN with:

``` r
install.packages("cardx")
```

You can install the development version of cards from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("insightsengineering/cardx")
```

## Examples

### Example ARD Creation

Example t-test:

``` r
library(cardx)

cards::ADSL |>
  # keep two treatment arms for the t-test calculation
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  cardx::ard_stats_t_test(by = ARM, variable = AGE)
```

    ## {cards} data frame: 14 x 9

    ##    group1 variable   context   stat_name stat_label      stat
    ## 1     ARM      AGE stats_t_…    estimate  Mean Dif…     0.828
    ## 2     ARM      AGE stats_t_…   estimate1  Group 1 …    75.209
    ## 3     ARM      AGE stats_t_…   estimate2  Group 2 …    74.381
    ## 4     ARM      AGE stats_t_…   statistic  t Statis…     0.655
    ## 5     ARM      AGE stats_t_…     p.value    p-value     0.513
    ## 6     ARM      AGE stats_t_…   parameter  Degrees …   167.362
    ## 7     ARM      AGE stats_t_…    conf.low  CI Lower…    -1.668
    ## 8     ARM      AGE stats_t_…   conf.high  CI Upper…     3.324
    ## 9     ARM      AGE stats_t_…      method     method Welch Tw…
    ## 10    ARM      AGE stats_t_… alternative  alternat… two.sided
    ## 11    ARM      AGE stats_t_…          mu    H0 Mean         0
    ## 12    ARM      AGE stats_t_…      paired  Paired t…     FALSE
    ## 13    ARM      AGE stats_t_…   var.equal  Equal Va…     FALSE
    ## 14    ARM      AGE stats_t_…  conf.level  CI Confi…      0.95

    ## ℹ 3 more variables: fmt_fn, warning, error

Note that the returned ARD contains the analysis results in addition to
the function parameters used to calculate the results allowing for
reproducible future analyses and further customization.

### Model Input

Some {cardx} functions accept regression model objects as input:

``` r
lm(AGE ~ ARM, data = cards::ADSL) |>
  ard_aod_wald_test()
```

Note that the [Analysis Results
Standard](https://www.cdisc.org/standards/foundational/analysis-results-standard)
should begin with a data set rather than a model object. To accomplish
this we include model construction helpers.

``` r
construct_model(
  data = cards::ADSL,
  formula = reformulate2("ARM", response = "AGE"),
  method = "lm"
) |>
  ard_aod_wald_test()
```

    ## {cards} data frame: 6 x 8

    ##      variable   context stat_name stat_label     stat fmt_fn
    ## 1 (Intercept) aod_wald…        df  Degrees …        1      1
    ## 2 (Intercept) aod_wald… statistic  Statistic 7126.713      1
    ## 3 (Intercept) aod_wald…   p.value    p-value        0      1
    ## 4         ARM aod_wald…        df  Degrees …        2      1
    ## 5         ARM aod_wald… statistic  Statistic    1.046      1
    ## 6         ARM aod_wald…   p.value    p-value    0.593      1

    ## ℹ 2 more variables: warning, error

## Additional Resources

- The best resources are the help documents accompanying each {cardx}
  function.
- Supporting documentation for both companion packages
  [{cards}](https://insightsengineering.github.io/cards/) and
  {[gtsummary](https://www.danieldsjoberg.com/gtsummary/index.html)}
  will be useful for understanding the ARD workflow and capabilities.
