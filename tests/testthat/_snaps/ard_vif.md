# ard_vif() works

    Code
      ard_vif(lm(AGE ~ ARM + SEX, data = cards::ADSL))
    Message
      {cards} data frame: 6 x 7
    Output
        variable context stat_name stat_label statistic
      1      ARM     vif      GVIF       GVIF  1.015675
      2      ARM     vif        df         df  2.000000
      3      ARM     vif     aGVIF  Adjusted…  1.003896
      4      SEX     vif      GVIF       GVIF  1.015675
      5      SEX     vif        df         df  1.000000
      6      SEX     vif     aGVIF  Adjusted…  1.007807
    Message
      i 2 more variables: warning, error

# ard_vif() appropriate errors are given for model with only 1 term

    Code
      ard_vif(lm(AGE ~ ARM, data = cards::ADSL))
    Message
      {cards} data frame: 2 x 7
    Output
                       variable context stat_name stat_label     error statistic
      1 ARMXanomeline High Dose     vif       VIF        VIF model co…      NULL
      2  ARMXanomeline Low Dose     vif       VIF        VIF model co…      NULL
    Message
      i 1 more variable: warning

