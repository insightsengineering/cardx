# ard_stats_kruskal_test() works

    Code
      as.data.frame(ard_stats_kruskal_test(cards::ADSL, by = "ARM", variables = "AGE"))
    Output
        group1 variable     context stat_name                           stat_label
      1    ARM      AGE kruskaltest statistic Kruskal-Wallis chi-squared Statistic
      2    ARM      AGE kruskaltest   p.value                              p-value
      3    ARM      AGE kruskaltest parameter                   Degrees of Freedom
      4    ARM      AGE kruskaltest    method                               method
                                stat fmt_fn warning error
      1                      1.63473      1    NULL  NULL
      2                    0.4415937      1    NULL  NULL
      3                            2      1    NULL  NULL
      4 Kruskal-Wallis rank sum test   NULL    NULL  NULL

