# ard_car_vif() works

    Code
      as.data.frame(ard_car_vif(lm(AGE ~ ARM + SEX, data = cards::ADSL)))
    Output
        variable context stat_name    stat_label     stat fmt_fn warning error
      1      ARM car_vif      GVIF          GVIF 1.015675      1    NULL  NULL
      2      ARM car_vif        df            df 2.000000      1    NULL  NULL
      3      ARM car_vif     aGVIF Adjusted GVIF 1.003896      1    NULL  NULL
      4      SEX car_vif      GVIF          GVIF 1.015675      1    NULL  NULL
      5      SEX car_vif        df            df 1.000000      1    NULL  NULL
      6      SEX car_vif     aGVIF Adjusted GVIF 1.007807      1    NULL  NULL

---

    Code
      as.data.frame(ard_car_vif(lm(AGE ~ BMIBL + EDUCLVL, data = cards::ADSL)))
    Output
        variable context stat_name stat_label     stat fmt_fn warning error
      1    BMIBL car_vif       VIF        VIF 1.010522      1    NULL  NULL
      2  EDUCLVL car_vif       VIF        VIF 1.010522      1    NULL  NULL

# ard_car_vif() appropriate errors are given for model with only 1 term

    Code
      as.data.frame(ard_car_vif(lm(AGE ~ ARM, data = cards::ADSL)))
    Output
        variable context stat_name    stat_label stat fmt_fn warning
      1      ARM car_vif       VIF           VIF NULL   NULL    NULL
      2      ARM car_vif      GVIF          GVIF NULL   NULL    NULL
      3      ARM car_vif     aGVIF Adjusted GVIF NULL   NULL    NULL
      4      ARM car_vif        df            df NULL   NULL    NULL
                                    error
      1 model contains fewer than 2 terms
      2 model contains fewer than 2 terms
      3 model contains fewer than 2 terms
      4 model contains fewer than 2 terms

# ard_vif() issues friendly messaging for incorrect object passed in/can't get terms of model

    Code
      ard_vif(cards::ADSL)
    Condition
      Error in `ard_vif()`:
      ! could not find function "ard_vif"

