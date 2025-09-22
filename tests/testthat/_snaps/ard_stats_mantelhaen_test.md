# ard_stats_mantelhaen_test() works

    Code
      print(ard_mantelhaentest, columns = "all")
    Message
      {cards} data frame: 8 x 10
    Output
        group1 group2 variable   context   stat_name stat_label      stat fmt_fun warning error
      1    ARM    SEX   AGEGR1 stats_ma…   statistic  Generali…     6.455       1              
      2    ARM    SEX   AGEGR1 stats_ma…     p.value    p-value     0.168       1              
      3    ARM    SEX   AGEGR1 stats_ma…   parameter  Degrees …         4       1              
      4    ARM    SEX   AGEGR1 stats_ma…      method     method Cochran-…    <fn>              
      5    ARM    SEX   AGEGR1 stats_ma… alternative  alternat… two.sided    <fn>              
      6    ARM    SEX   AGEGR1 stats_ma…     correct  Continui…      TRUE    <fn>              
      7    ARM    SEX   AGEGR1 stats_ma…       exact  Exact Co…     FALSE    <fn>              
      8    ARM    SEX   AGEGR1 stats_ma…  conf.level  CI Confi…      0.95       1              

---

    Code
      print(ard_mantelhaentest, columns = "all")
    Message
      {cards} data frame: 8 x 10
    Output
        group1 group2 variable   context   stat_name stat_label      stat fmt_fun warning error
      1    ARM    SEX   AGEGR1 stats_ma…   statistic  Mantel-H…     6.455       1              
      2    ARM    SEX   AGEGR1 stats_ma…     p.value    p-value     0.168       1              
      3    ARM    SEX   AGEGR1 stats_ma…   parameter  Degrees …         4       1              
      4    ARM    SEX   AGEGR1 stats_ma…      method     method Cochran-…    <fn>              
      5    ARM    SEX   AGEGR1 stats_ma… alternative  alternat…      less    <fn>              
      6    ARM    SEX   AGEGR1 stats_ma…     correct  Continui…     FALSE    <fn>              
      7    ARM    SEX   AGEGR1 stats_ma…       exact  Exact Co…      TRUE    <fn>              
      8    ARM    SEX   AGEGR1 stats_ma…  conf.level  CI Confi…       0.9       1              

