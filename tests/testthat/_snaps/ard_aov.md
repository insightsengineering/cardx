# ard_aov() works

    Code
      as.data.frame(ard_aov(lm(AGE ~ ARM + SEX, data = cards::ADSL)))
    Output
         variable context stat_name             stat_label      stat warning error
      1       ARM     aov     sumsq         Sum of Squares  71.38574    NULL  NULL
      2       ARM     aov        df     Degrees of Freedom         2    NULL  NULL
      3       ARM     aov    meansq Mean of Sum of Squares  35.69287    NULL  NULL
      4       ARM     aov statistic              Statistic 0.5235002    NULL  NULL
      5       ARM     aov   p.value                p-value 0.5930912    NULL  NULL
      6       SEX     aov     sumsq         Sum of Squares  87.40947    NULL  NULL
      7       SEX     aov        df     Degrees of Freedom         1    NULL  NULL
      8       SEX     aov    meansq Mean of Sum of Squares  87.40947    NULL  NULL
      9       SEX     aov statistic              Statistic  1.282017    NULL  NULL
      10      SEX     aov   p.value                p-value 0.2586091    NULL  NULL

