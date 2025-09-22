# ard_tabulate_abnormal() works

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 18 x 11
    Output
         group1 group1_level variable variable_level   context stat_name stat_label  stat fmt_fun warning error
      1    TRTA      Placebo  LBNRIND            Low categori…         n          n     2       0              
      2    TRTA      Placebo  LBNRIND            Low categori…         N          N     7       0              
      3    TRTA      Placebo  LBNRIND            Low categori…         p          % 0.286    <fn>              
      4    TRTA      Placebo  LBNRIND           High categori…         n          n     3       0              
      5    TRTA      Placebo  LBNRIND           High categori…         N          N     7       0              
      6    TRTA      Placebo  LBNRIND           High categori…         p          % 0.429    <fn>              
      7    TRTA    Xanomeli…  LBNRIND            Low categori…         n          n     4       0              
      8    TRTA    Xanomeli…  LBNRIND            Low categori…         N          N     7       0              
      9    TRTA    Xanomeli…  LBNRIND            Low categori…         p          % 0.571    <fn>              
      10   TRTA    Xanomeli…  LBNRIND           High categori…         n          n     3       0              
      11   TRTA    Xanomeli…  LBNRIND           High categori…         N          N     7       0              
      12   TRTA    Xanomeli…  LBNRIND           High categori…         p          % 0.429    <fn>              
      13   TRTA    Xanomeli…  LBNRIND            Low categori…         n          n     4       0              
      14   TRTA    Xanomeli…  LBNRIND            Low categori…         N          N     6       0              
      15   TRTA    Xanomeli…  LBNRIND            Low categori…         p          % 0.667    <fn>              
      16   TRTA    Xanomeli…  LBNRIND           High categori…         n          n     3       0              
      17   TRTA    Xanomeli…  LBNRIND           High categori…         N          N     6       0              
      18   TRTA    Xanomeli…  LBNRIND           High categori…         p          %   0.5    <fn>              

---

    Code
      print(ard_tabulate_abnormal(adlb, postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH"), other = "OTHER")), columns = "all")
    Message
      Abnormality "low" created by merging levels: "LOW", "LOW LOW"
      Abnormality "high" created by merging levels: "HIGH", "HIGH HIGH"
      Abnormality "other" created from level: "OTHER"
      {cards} data frame: 9 x 9
    Output
        variable variable_level   context stat_name stat_label stat fmt_fun warning error
      1  LBNRIND            low categori…         n          n   10       0              
      2  LBNRIND            low categori…         N          N   20       0              
      3  LBNRIND            low categori…         p          %  0.5    <fn>              
      4  LBNRIND           high categori…         n          n    9       0              
      5  LBNRIND           high categori…         N          N   20       0              
      6  LBNRIND           high categori…         p          % 0.45    <fn>              
      7  LBNRIND          other categori…         n          n    0       0              
      8  LBNRIND          other categori…         N          N   20       0              
      9  LBNRIND          other categori…         p          %    0    <fn>              

---

    Code
      print(ard_tabulate_abnormal(adlb, postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID, by = TRTA, excl_baseline_abn = FALSE), columns = "all")
    Message
      Abnormality "Low" created from level: "LOW"
      Abnormality "High" created from level: "HIGH"
      {cards} data frame: 18 x 11
    Output
         group1 group1_level variable variable_level   context stat_name stat_label  stat fmt_fun warning error
      1    TRTA      Placebo  LBNRIND            Low categori…         n          n     2       0              
      2    TRTA      Placebo  LBNRIND            Low categori…         N          N     7       0              
      3    TRTA      Placebo  LBNRIND            Low categori…         p          % 0.286    <fn>              
      4    TRTA      Placebo  LBNRIND           High categori…         n          n     3       0              
      5    TRTA      Placebo  LBNRIND           High categori…         N          N     7       0              
      6    TRTA      Placebo  LBNRIND           High categori…         p          % 0.429    <fn>              
      7    TRTA    Xanomeli…  LBNRIND            Low categori…         n          n     4       0              
      8    TRTA    Xanomeli…  LBNRIND            Low categori…         N          N     7       0              
      9    TRTA    Xanomeli…  LBNRIND            Low categori…         p          % 0.571    <fn>              
      10   TRTA    Xanomeli…  LBNRIND           High categori…         n          n     3       0              
      11   TRTA    Xanomeli…  LBNRIND           High categori…         N          N     7       0              
      12   TRTA    Xanomeli…  LBNRIND           High categori…         p          % 0.429    <fn>              
      13   TRTA    Xanomeli…  LBNRIND            Low categori…         n          n     4       0              
      14   TRTA    Xanomeli…  LBNRIND            Low categori…         N          N     6       0              
      15   TRTA    Xanomeli…  LBNRIND            Low categori…         p          % 0.667    <fn>              
      16   TRTA    Xanomeli…  LBNRIND           High categori…         n          n     3       0              
      17   TRTA    Xanomeli…  LBNRIND           High categori…         N          N     6       0              
      18   TRTA    Xanomeli…  LBNRIND           High categori…         p          %   0.5    <fn>              

# ard_tabulate_abnormal() errors are handled correctly

    Code
      res <- ard_tabulate_abnormal(adlb, postbaseline = LBNRIND, baseline = BNRIND,
        id = USUBJID, by = TRTA, abnormal = list("HIGH", "LOW"))
    Condition
      Error in `ard_tabulate_abnormal()`:
      ! `abnormal` must be a named list, where each name corresponds to a different abnormality/direction.

---

    Code
      res <- ard_tabulate_abnormal(adlb, postbaseline = LBNRIND, baseline = BNRIND,
        id = USUBJID, by = TRTA, abnormal = list(high = 1:5, low = 0))
    Condition
      Error in `ard_tabulate_abnormal()`:
      ! Each abnormal level of `LBNRIND` specified via `abnormal` must be a <string>.

