# shuffle_ard fills missing group levels if the group is meaningful

    Code
      as.data.frame(cards::shuffle_ard(cards::bind_ard(ard_chisqtest(data = adsl_sub,
        by = "ARM", variable = "AGEGR1"), ard_chisqtest(data = adsl_sub, by = "SEX",
        variable = "AGEGR1"))))
    Output
                ARM         SEX variable   context stat_name    statistic
      1 Overall ARM        <NA>   AGEGR1 chisqtest statistic 5.079442e+00
      2 Overall ARM        <NA>   AGEGR1 chisqtest   p.value 7.888842e-02
      3 Overall ARM        <NA>   AGEGR1 chisqtest parameter 2.000000e+00
      4 Overall ARM        <NA>   AGEGR1 chisqtest         B 2.000000e+03
      5        <NA> Overall SEX   AGEGR1 chisqtest statistic 1.039442e+00
      6        <NA> Overall SEX   AGEGR1 chisqtest   p.value 5.946864e-01
      7        <NA> Overall SEX   AGEGR1 chisqtest parameter 2.000000e+00
      8        <NA> Overall SEX   AGEGR1 chisqtest         B 2.000000e+03

