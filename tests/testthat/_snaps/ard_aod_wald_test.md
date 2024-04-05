# ard_aod_wald_test() works

    Code
      glm_ard_aod_wald_test[, 1:6]
    Message
      {cards} data frame: 6 x 6
    Output
           variable   context stat_name stat_label     stat fmt_fn
      1 (Intercept) aod_wald…        df  Degrees …        1      1
      2 (Intercept) aod_wald… statistic  Statistic 7126.713      1
      3 (Intercept) aod_wald…   p.value    p-value        0      1
      4         ARM aod_wald…        df  Degrees …        2      1
      5         ARM aod_wald… statistic  Statistic    1.046      1
      6         ARM aod_wald…   p.value    p-value    0.593      1

---

    Code
      dplyr::select(ard_aod_wald_test(cards::ADSL), c(context, error))
    Output
      Objects of class `NULL` are currently not supported.
      Objects of class `NULL` are currently not supported.
    Message
      {cards} data frame: 3 x 2
    Output
          context     error
      1 aod_wald… Error: !…
      2 aod_wald… Error: !…
      3 aod_wald… Error: !…
