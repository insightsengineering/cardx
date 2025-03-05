# ard_stats_mantelhaen_test.data.frame() works

    Code
      print(ard_mantelhaentest, columns = "all")
    Message
      {cards} data frame: 8 x 10
    Output
        group1 group2 variable   context   stat_name stat_label      stat fmt_fn warning error
      1    ARM    SEX   AGEGR1 stats_ma…   statistic  Generali…     6.455      1              
      2    ARM    SEX   AGEGR1 stats_ma…     p.value    p-value     0.168      1              
      3    ARM    SEX   AGEGR1 stats_ma…   parameter  Degrees …         4      1              
      4    ARM    SEX   AGEGR1 stats_ma…      method     method Cochran-…   NULL              
      5    ARM    SEX   AGEGR1 stats_ma… alternative  alternat… two.sided   NULL              
      6    ARM    SEX   AGEGR1 stats_ma…     correct  Continui…      TRUE   NULL              
      7    ARM    SEX   AGEGR1 stats_ma…       exact  Exact Co…     FALSE   NULL              
      8    ARM    SEX   AGEGR1 stats_ma…  conf.level  CI Confi…      0.95      1              

# ard_stats_mantelhaen_test.array() works

    Code
      ard_mantelhaentest <- ard_stats_mantelhaen_test(bad_array)
    Condition
      Error in `ard_stats_mantelhaen_test()`:
      ! When `data` is an <array> it must be 3-dimensional, but `data` is currently 2-dimensional.

---

    Code
      ard_mantelhaentest <- ard_stats_mantelhaen_test(bad_array)
    Condition
      Error in `ard_stats_mantelhaen_test()`:
      ! The array given as `data` is unnamed but must have 3 named dimensions. The names will be assigned as the `by`, `variables`, and `strata` column names, respectively.

