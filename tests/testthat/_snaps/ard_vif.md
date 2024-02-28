# ard_vif() works

    Code
      ard_vif(lm(AGE ~ ARM + SEX, data = cards::ADSL))
    Message
      {cards} data frame: 6 x 7
    Output
        variable context stat_name stat_label  stat
      1      ARM     vif      GVIF       GVIF 1.016
      2      ARM     vif        df         df     2
      3      ARM     vif     aGVIF  Adjusted… 1.004
      4      SEX     vif      GVIF       GVIF 1.016
      5      SEX     vif        df         df     1
      6      SEX     vif     aGVIF  Adjusted… 1.008
    Message
      i 2 more variables: warning, error

# ard_vif() appropriate errors are given for model with only 1 term

    Code
      ard_vif(lm(AGE ~ ARM, data = cards::ADSL))
    Message
      {cards} data frame: 2 x 7
    Output
                       variable context stat_name stat_label stat     error
      1 ARMXanomeline High Dose     vif       VIF        VIF      model co…
      2  ARMXanomeline Low Dose     vif       VIF        VIF      model co…
    Message
      i 1 more variable: warning

