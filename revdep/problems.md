# gtsummary

<details>

* Version: 2.0.3
* GitHub: https://github.com/ddsjoberg/gtsummary
* Source code: https://github.com/cran/gtsummary
* Date/Publication: 2024-10-04 19:30:02 UTC
* Number of recursive dependencies: 186

Run `revdepcheck::revdep_details(, "gtsummary")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘spelling.R’
      Comparing ‘spelling.Rout’ to ‘spelling.Rout.save’ ... OK
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 1573 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-add_ci.tbl_svysummary.R:724:3'): add_ci() correctly handles dichotomous variables ──
    ...
      
      `actual$response` is a character vector ('0')
      `expected$response` is an integer vector (0)
      
      `actual$grade` is a character vector ('III')
      `expected$grade` is an S3 object of class <factor>, an integer vector
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 1573 ]
      Error: Test failures
      Execution halted
    ```

