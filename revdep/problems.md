# gtsummary

<details>

* Version: 2.2.0
* GitHub: https://github.com/ddsjoberg/gtsummary
* Source code: https://github.com/cran/gtsummary
* Date/Publication: 2025-04-14 10:30:02 UTC
* Number of recursive dependencies: 204

Run `revdepcheck::revdep_details(, "gtsummary")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gtsummary-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_ci
    > ### Title: Add CI Column
    > ### Aliases: add_ci add_ci.tbl_summary
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
     29. │               └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
     30. │                 └─mask$eval_all_mutate(quo)
     31. │                   └─dplyr (local) eval()
     32. ├─base::ifelse(...)
     33. ├─fmt_fn
     34. ├─rlang:::`$.rlang_data_pronoun`(.data, fmt_fn)
     35. │ └─rlang:::data_pronoun_get(...)
     36. └─rlang:::abort_data_pronoun(x, call = y)
     37.   └─rlang::abort(msg, "rlang_error_data_pronoun_not_found", call = call)
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
      + Condition
      +   Warning:
      +   The `fmt_fn` argument of `ard_categorical()` is deprecated as of cards 0.6.1.
      +   i Please use the `fmt_fun` argument instead.
    ...
      +   The `fmt_fn` argument of `ard_dichotomous()` is deprecated as of cards 0.6.1.
      +   i Please use the `fmt_fun` argument instead.
      and 15 more ...
      
      * Run `testthat::snapshot_accept('theme_gtsummary')` to accept the change.
      * Run `testthat::snapshot_review('theme_gtsummary')` to interactively review the change.
      
      [ FAIL 233 | WARN 2367 | SKIP 0 | PASS 1361 ]
      Error: Test failures
      Execution halted
    ```

