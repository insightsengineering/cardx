# gtsummary

<details>

* Version: 2.0.4
* GitHub: https://github.com/ddsjoberg/gtsummary
* Source code: https://github.com/cran/gtsummary
* Date/Publication: 2024-11-30 18:40:02 UTC
* Number of recursive dependencies: 190

Run `revdepcheck::revdep_details(, "gtsummary")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gtsummary-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_n.tbl_survfit
    > ### Title: Add N
    > ### Aliases: add_n.tbl_survfit
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    +   add_n()
    + ## Don't show: 
    + }) # examplesIf
    > library(survival)
    > fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
    > fit2 <- survfit(Surv(ttdeath, death) ~ trt, trial)
    > add_n(tbl_survfit(list(fit1, fit2), times = c(12, 24)))
    Error in loadNamespace(x) : there is no package called ‘cardx’
    Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘spelling.R’
      Comparing ‘spelling.Rout’ to ‘spelling.Rout.save’ ...6,11c6
    < Potential spelling errors:
    <   WORD    FOUND IN
    < coxph   tbl_uvregression.Rd:66
    < glm     tbl_uvregression.Rd:66,143
    < lm      tbl_uvregression.Rd:66,142
    < If these are false positive, run `spelling::update_wordlist()`.All Done!
    ---
    > All Done!
    ...
      Error in `modify_column_hide(add_difference(tbl_svysummary(svy_trial, by = trt, 
          include = age, label = age ~ "Age", missing = "no")), c("stat_2"))`: The package "cardx" (>= 0.2.2) is required.
      Backtrace:
          ▆
       1. └─testthat::expect_snapshot(...) at test-theme_gtsummary.R:245:3
       2.   └─rlang::cnd_signal(state$error)
      
      [ FAIL 168 | WARN 1 | SKIP 28 | PASS 883 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘cardx’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cardx’
    ```

# smdi

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/smdi
* Date/Publication: 2024-10-04 07:10:02 UTC
* Number of recursive dependencies: 221

Run `revdepcheck::revdep_details(, "smdi")` for more info

</details>

## Newly broken

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘cardx’
    ```

## In both

*   checking running R code from vignettes ...
    ```
      ‘a_data_generation.Rmd’ using ‘UTF-8’... failed
      ‘b_routine_diagnostics.Rmd’ using ‘UTF-8’... failed
      ‘c_multivariate_missingness.Rmd’ using ‘UTF-8’... OK
      ‘d_narfcs_sensitivity_analysis.Rmd’ using ‘UTF-8’... OK
      ‘smdi.Rmd’ using ‘UTF-8’... OK
     WARNING
    Errors in running code in vignettes:
    when running code in ‘a_data_generation.Rmd’
      ...
    The following objects are masked from ‘package:base’:
    ...
    > library(here)
    here() starts at /private/var/folders/6f/gdjf_vxj2wl3jhmxdkd1hd_w0000gn/T/RtmpRgZtpU/file1b6a67ad4480/vignettes
    
    > library(knitr)
    
    > include_graphics(here("vignettes", "smdi_diagnose_table.png"))
    
      When sourcing ‘b_routine_diagnostics.R’:
    Error: Cannot find the file(s): "/private/var/folders/6f/gdjf_vxj2wl3jhmxdkd1hd_w0000gn/T/RtmpRgZtpU/file1b6a67ad4480/vignettes/vignettes/smdi_diagnose_table.png"
    Execution halted
    ```

# tidycmprsk

<details>

* Version: 1.1.0
* GitHub: https://github.com/MSKCC-Epi-Bio/tidycmprsk
* Source code: https://github.com/cran/tidycmprsk
* Date/Publication: 2024-08-17 04:10:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::revdep_details(, "tidycmprsk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidycmprsk-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gtsummary_s3_methods
    > ### Title: gtsummary methods
    > ### Aliases: gtsummary_s3_methods tbl_regression.tidycrr
    > ###   global_pvalue_fun.tidycrr
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─gtsummary::as_gt(...)
     2. │ └─gtsummary:::check_class(x, "gtsummary")
     3. ├─gtsummary::add_global_p(...)
     4. └─gtsummary:::add_global_p.tbl_regression(...)
     5.   └─gtsummary:::.calculate_anova_fun(...)
     6.     └─cli::cli_abort(...)
     7.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘spelling.R’
      Comparing ‘spelling.Rout’ to ‘spelling.Rout.save’ ... OK
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7. ├─gtsummary::add_global_p(tbl_regression(mod))
        8. └─gtsummary:::add_global_p.tbl_regression(tbl_regression(mod))
        9.   └─gtsummary:::.calculate_anova_fun(...)
       10.     └─cli::cli_abort(...)
    ...
      ── Error ('test-gtsummary_s3_methods.R:17:3'): global_pvalue_fun.tidycrr() ─────
      Error in `as.data.frame.default(tbl, col_labels = FALSE)`: cannot coerce class '"function"' to a data.frame
      Backtrace:
          ▆
       1. └─testthat::expect_snapshot(as.data.frame(tbl, col_labels = FALSE)) at test-gtsummary_s3_methods.R:17:3
       2.   └─rlang::cnd_signal(state$error)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 106 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘cardx’
    ```

