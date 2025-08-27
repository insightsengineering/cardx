# gtsummary

<details>

* Version: 2.3.0
* GitHub: https://github.com/ddsjoberg/gtsummary
* Source code: https://github.com/cran/gtsummary
* Date/Publication: 2025-07-03 17:50:02 UTC
* Number of recursive dependencies: 204

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

## In both

*   checking tests ...
    ```
      Running ‘spelling.R’
      Comparing ‘spelling.Rout’ to ‘spelling.Rout.save’ ... OK
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       19. ├─gtsummary:::map(.data$data, .tbl_fun, ...)
       20. │ └─base::lapply(.x, .f, ...)
       21. │   └─gtsummary (local) FUN(X[[i]], ...)
       22. │     └─gtsummary::tbl_svysummary(...)
    ...
       24. │         ├─base::suppressWarnings(...)
       25. │         │ └─base::withCallingHandlers(...)
       26. │         └─rlang::check_installed(...)
       27. │           └─base::stop(cnd)
       28. └─dplyr (local) `<fn>`(`<rlb_r___>`)
       29.   └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 155 | WARN 0 | SKIP 55 | PASS 759 ]
      Error: Test failures
      Execution halted
    ```

# tidycmprsk

<details>

* Version: 1.1.0
* GitHub: https://github.com/MSKCC-Epi-Bio/tidycmprsk
* Source code: https://github.com/cran/tidycmprsk
* Date/Publication: 2024-08-17 04:10:02 UTC
* Number of recursive dependencies: 115

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

