# shuffle_ard fills missing group levels if the group is meaningful

    Code
      as.data.frame(cards::bind_ard(ard_kruskaltest(data = adsl_sub, by = "ARM",
        variable = "AGEGR1"), ard_kruskaltest(data = adsl_sub, by = "SEX", variable = "AGEGR1")))
    Output
         group1 variable     context stat_name                           stat_label
      1     ARM   AGEGR1 kruskaltest statistic Kruskal-Wallis chi-squared Statistic
      2     ARM   AGEGR1 kruskaltest   p.value                              p-value
      3     ARM   AGEGR1 kruskaltest parameter                   Degrees of Freedom
      4     ARM   AGEGR1 kruskaltest    method                               method
      5     ARM   AGEGR1 kruskaltest         x                                    x
      6     ARM   AGEGR1 kruskaltest         g                                    g
      7     ARM   AGEGR1 kruskaltest       ...                                  ...
      8     SEX   AGEGR1 kruskaltest statistic Kruskal-Wallis chi-squared Statistic
      9     SEX   AGEGR1 kruskaltest   p.value                              p-value
      10    SEX   AGEGR1 kruskaltest parameter                   Degrees of Freedom
      11    SEX   AGEGR1 kruskaltest    method                               method
      12    SEX   AGEGR1 kruskaltest         x                                    x
      13    SEX   AGEGR1 kruskaltest         g                                    g
      14    SEX   AGEGR1 kruskaltest       ...                                  ...
                            statistic statistic_fmt_fn warning error
      1                      5.045766                1    NULL  NULL
      2                    0.02468619                1    NULL  NULL
      3                             1                1    NULL  NULL
      4  Kruskal-Wallis rank sum test             NULL    NULL  NULL
      5                                           NULL    NULL  NULL
      6                                           NULL    NULL  NULL
      7                                           NULL    NULL  NULL
      8                      1.023611                1    NULL  NULL
      9                     0.3116639                1    NULL  NULL
      10                            1                1    NULL  NULL
      11 Kruskal-Wallis rank sum test             NULL    NULL  NULL
      12                                          NULL    NULL  NULL
      13                                          NULL    NULL  NULL
      14                                          NULL    NULL  NULL

