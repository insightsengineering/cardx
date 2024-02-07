# shuffle_ard fills missing group levels if the group is meaningful

    Code
      as.data.frame(cards::shuffle_ard(cards::bind_ard(ard_kwtest(data = adsl_sub,
        by = "ARM", variable = "AGEGR1"), ard_kwtest(data = adsl_sub, by = "SEX",
        variable = "AGEGR1"))))
    Output
                 ARM         SEX variable           context  stat_name  statistic
      1  Overall ARM        <NA>   AGEGR1 kruskalwallistest  statistic 5.04576634
      2  Overall ARM        <NA>   AGEGR1 kruskalwallistest    p.value 0.02468619
      3  Overall ARM        <NA>   AGEGR1 kruskalwallistest  parameter 1.00000000
      4  Overall ARM        <NA>   AGEGR1 kruskalwallistest         mu 0.00000000
      5  Overall ARM        <NA>   AGEGR1 kruskalwallistest conf.level 0.95000000
      6         <NA> Overall SEX   AGEGR1 kruskalwallistest  statistic 1.02361124
      7         <NA> Overall SEX   AGEGR1 kruskalwallistest    p.value 0.31166394
      8         <NA> Overall SEX   AGEGR1 kruskalwallistest  parameter 1.00000000
      9         <NA> Overall SEX   AGEGR1 kruskalwallistest         mu 0.00000000
      10        <NA> Overall SEX   AGEGR1 kruskalwallistest conf.level 0.95000000

