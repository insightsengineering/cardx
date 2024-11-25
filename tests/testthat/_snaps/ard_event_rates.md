# ard_event_rates() works with default settings

    Code
      print(res, n = 20, columns = "all")
    Message
      {cards} data frame: 207 x 11
    Output
         group1 group1_level variable variable_level   context stat_name stat_label  stat fmt_fn warning error
      1    TRTA      Placebo    AESOC      CARDIAC … event_ra…         n          n    13      0              
      2    TRTA      Placebo    AESOC      CARDIAC … event_ra…         N          N   301      0              
      3    TRTA      Placebo    AESOC      CARDIAC … event_ra…         p          % 0.043   <fn>              
      4    TRTA      Placebo    AESOC      CONGENIT… event_ra…         n          n     0      0              
      5    TRTA      Placebo    AESOC      CONGENIT… event_ra…         N          N   301      0              
      6    TRTA      Placebo    AESOC      CONGENIT… event_ra…         p          %     0   <fn>              
      7    TRTA      Placebo    AESOC      EAR AND … event_ra…         n          n     1      0              
      8    TRTA      Placebo    AESOC      EAR AND … event_ra…         N          N   301      0              
      9    TRTA      Placebo    AESOC      EAR AND … event_ra…         p          % 0.003   <fn>              
      10   TRTA      Placebo    AESOC      EYE DISO… event_ra…         n          n     4      0              
      11   TRTA      Placebo    AESOC      EYE DISO… event_ra…         N          N   301      0              
      12   TRTA      Placebo    AESOC      EYE DISO… event_ra…         p          % 0.013   <fn>              
      13   TRTA      Placebo    AESOC      GASTROIN… event_ra…         n          n    17      0              
      14   TRTA      Placebo    AESOC      GASTROIN… event_ra…         N          N   301      0              
      15   TRTA      Placebo    AESOC      GASTROIN… event_ra…         p          % 0.056   <fn>              
      16   TRTA      Placebo    AESOC      GENERAL … event_ra…         n          n    21      0              
      17   TRTA      Placebo    AESOC      GENERAL … event_ra…         N          N   301      0              
      18   TRTA      Placebo    AESOC      GENERAL … event_ra…         p          %  0.07   <fn>              
      19   TRTA      Placebo    AESOC      HEPATOBI… event_ra…         n          n     1      0              
      20   TRTA      Placebo    AESOC      HEPATOBI… event_ra…         N          N   301      0              
    Message
      i 187 more rows
      i Use `print(n = ...)` to see more rows

---

    Code
      print(ard_event_rates(group_by(cards::ADAE, TRTA), variables = AESOC, id = USUBJID, denominator = dplyr::rename(cards::ADSL, TRTA = ARM)), n = 20, columns = "all")
    Message
      {cards} data frame: 207 x 11
    Output
         group1 group1_level variable variable_level   context stat_name stat_label  stat fmt_fn warning error
      1    TRTA      Placebo    AESOC      CARDIAC … event_ra…         n          n    13      0              
      2    TRTA      Placebo    AESOC      CARDIAC … event_ra…         N          N    86      0              
      3    TRTA      Placebo    AESOC      CARDIAC … event_ra…         p          % 0.151   <fn>              
      4    TRTA      Placebo    AESOC      CONGENIT… event_ra…         n          n     0      0              
      5    TRTA      Placebo    AESOC      CONGENIT… event_ra…         N          N    86      0              
      6    TRTA      Placebo    AESOC      CONGENIT… event_ra…         p          %     0   <fn>              
      7    TRTA      Placebo    AESOC      EAR AND … event_ra…         n          n     1      0              
      8    TRTA      Placebo    AESOC      EAR AND … event_ra…         N          N    86      0              
      9    TRTA      Placebo    AESOC      EAR AND … event_ra…         p          % 0.012   <fn>              
      10   TRTA      Placebo    AESOC      EYE DISO… event_ra…         n          n     4      0              
      11   TRTA      Placebo    AESOC      EYE DISO… event_ra…         N          N    86      0              
      12   TRTA      Placebo    AESOC      EYE DISO… event_ra…         p          % 0.047   <fn>              
      13   TRTA      Placebo    AESOC      GASTROIN… event_ra…         n          n    17      0              
      14   TRTA      Placebo    AESOC      GASTROIN… event_ra…         N          N    86      0              
      15   TRTA      Placebo    AESOC      GASTROIN… event_ra…         p          % 0.198   <fn>              
      16   TRTA      Placebo    AESOC      GENERAL … event_ra…         n          n    21      0              
      17   TRTA      Placebo    AESOC      GENERAL … event_ra…         N          N    86      0              
      18   TRTA      Placebo    AESOC      GENERAL … event_ra…         p          % 0.244   <fn>              
      19   TRTA      Placebo    AESOC      HEPATOBI… event_ra…         n          n     1      0              
      20   TRTA      Placebo    AESOC      HEPATOBI… event_ra…         N          N    86      0              
    Message
      i 187 more rows
      i Use `print(n = ...)` to see more rows

# ard_event_rates(statistic) works

    Code
      ard_event_rates(cards::ADAE, variables = SEX, id = USUBJID, by = TRTA, denominator = dplyr::rename(cards::ADSL, TRTA = ARM), statistic = ~"n")
    Message
      {cards} data frame: 6 x 11
    Output
        group1 group1_level variable variable_level stat_name stat_label stat
      1   TRTA      Placebo      SEX              F         n          n   40
      2   TRTA      Placebo      SEX              M         n          n   29
      3   TRTA    Xanomeli…      SEX              F         n          n   37
      4   TRTA    Xanomeli…      SEX              M         n          n   42
      5   TRTA    Xanomeli…      SEX              F         n          n   44
      6   TRTA    Xanomeli…      SEX              M         n          n   33
    Message
      i 4 more variables: context, fmt_fn, warning, error

# ard_event_rates(ordered) works

    Code
      print(res, n = 20, columns = "all")
    Message
      {cards} data frame: 27 x 11
    Output
         group1 group1_level variable variable_level   context stat_name stat_label  stat fmt_fn warning error
      1    TRTA      Placebo    AESEV           MILD event_ra…         n          n    36      0              
      2    TRTA      Placebo    AESEV           MILD event_ra…         N          N    86      0              
      3    TRTA      Placebo    AESEV           MILD event_ra…         p          % 0.419   <fn>              
      4    TRTA      Placebo    AESEV       MODERATE event_ra…         n          n    26      0              
      5    TRTA      Placebo    AESEV       MODERATE event_ra…         N          N    86      0              
      6    TRTA      Placebo    AESEV       MODERATE event_ra…         p          % 0.302   <fn>              
      7    TRTA      Placebo    AESEV         SEVERE event_ra…         n          n     7      0              
      8    TRTA      Placebo    AESEV         SEVERE event_ra…         N          N    86      0              
      9    TRTA      Placebo    AESEV         SEVERE event_ra…         p          % 0.081   <fn>              
      10   TRTA    Xanomeli…    AESEV           MILD event_ra…         n          n    22      0              
      11   TRTA    Xanomeli…    AESEV           MILD event_ra…         N          N    84      0              
      12   TRTA    Xanomeli…    AESEV           MILD event_ra…         p          % 0.262   <fn>              
      13   TRTA    Xanomeli…    AESEV       MODERATE event_ra…         n          n    49      0              
      14   TRTA    Xanomeli…    AESEV       MODERATE event_ra…         N          N    84      0              
      15   TRTA    Xanomeli…    AESEV       MODERATE event_ra…         p          % 0.583   <fn>              
      16   TRTA    Xanomeli…    AESEV         SEVERE event_ra…         n          n     8      0              
      17   TRTA    Xanomeli…    AESEV         SEVERE event_ra…         N          N    84      0              
      18   TRTA    Xanomeli…    AESEV         SEVERE event_ra…         p          % 0.095   <fn>              
      19   TRTA    Xanomeli…    AESEV           MILD event_ra…         n          n    19      0              
      20   TRTA    Xanomeli…    AESEV           MILD event_ra…         N          N    84      0              
    Message
      i 7 more rows
      i Use `print(n = ...)` to see more rows

# ard_event_rates() errors with incomplete factor columns

    Code
      ard_event_rates(dplyr::mutate(cards::ADAE, AESOC = factor(AESOC, levels = character(
        0))), variables = AESOC, id = USUBJID, by = TRTA)
    Message
      * Removing 1191 rows from `data` with NA or NaN values in "TRTA" and "AESOC" columns.
    Condition
      Error in `ard_event_rates()`:
      ! Factors with empty "levels" attribute are not allowed, which was identified in column "AESOC".

---

    Code
      ard_event_rates(dplyr::mutate(cards::ADAE, SEX = factor(SEX, levels = c("F",
        "M", NA), exclude = NULL)), variables = SEX, id = USUBJID, by = TRTA)
    Condition
      Error in `ard_event_rates()`:
      ! Factors with NA levels are not allowed, which are present in column "SEX".

# ard_event_rates() works without any variables

    Code
      ard_event_rates(data = cards::ADAE, variables = starts_with("xxxx"), id = USUBJID,
      by = c(TRTA, AESEV))
    Message
      {cards} data frame: 0 x 0
    Output
      data frame with 0 columns and 0 rows

