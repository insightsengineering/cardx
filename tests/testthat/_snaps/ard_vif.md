# ard_vif() works

    Code
      as.data.frame(ard_vif(lm(AGE ~ ARM + SEX, data = cards::ADSL)))
    Output
        variable context stat_name    stat_label     stat fmt_fn warning error
      1      ARM     vif      GVIF          GVIF 1.015675      1    NULL  NULL
      2      ARM     vif        df            df 2.000000      1    NULL  NULL
      3      ARM     vif     aGVIF Adjusted GVIF 1.003896      1    NULL  NULL
      4      SEX     vif      GVIF          GVIF 1.015675      1    NULL  NULL
      5      SEX     vif        df            df 1.000000      1    NULL  NULL
      6      SEX     vif     aGVIF Adjusted GVIF 1.007807      1    NULL  NULL

---

    Code
      as.data.frame(ard_vif(lm(AGE ~ BMIBL + EDUCLVL, data = cards::ADSL)))
    Output
        variable context stat_name stat_label     stat fmt_fn warning error
      1    BMIBL     vif       VIF        VIF 1.010522      1    NULL  NULL
      2  EDUCLVL     vif       VIF        VIF 1.010522      1    NULL  NULL

# ard_vif() appropriate errors are given for model with only 1 term

    Code
      as.data.frame(ard_vif(lm(AGE ~ ARM, data = cards::ADSL)))
    Output
        variable context stat_name    stat_label stat fmt_fn warning
      1      ARM     vif       VIF           VIF NULL   NULL    NULL
      2      ARM     vif      GVIF          GVIF NULL   NULL    NULL
      3      ARM     vif     aGVIF Adjusted GVIF NULL   NULL    NULL
      4      ARM     vif        df            df NULL   NULL    NULL
                                    error
      1 model contains fewer than 2 terms
      2 model contains fewer than 2 terms
      3 model contains fewer than 2 terms
      4 model contains fewer than 2 terms

