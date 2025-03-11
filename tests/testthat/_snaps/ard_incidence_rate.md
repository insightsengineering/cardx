# ard_incidence_rate() works

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label   stat fmt_fn warning error
      1     AVAL incidenc…         estimate  Estimate…  0.605      1              
      2     AVAL incidenc…         conf.low  CI Lower…  0.488      1              
      3     AVAL incidenc…        conf.high  CI Upper…  0.723      1              
      4     AVAL incidenc…        conf.type    CI Type normal   <fn>              
      5     AVAL incidenc…       conf.level  CI Confi…   0.95      1              
      6     AVAL incidenc… tot_person_years  Person-Y…  16853      1              
      7     AVAL incidenc…         n_events  Number o…    102      1              
      8     AVAL incidenc…      n_unique_id  Number o…    102      0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label     stat fmt_fn warning error
      1     AVAL incidenc…         estimate  Estimate…    0.726      1              
      2     AVAL incidenc…         conf.low  CI Lower…    0.585      1              
      3     AVAL incidenc…        conf.high  CI Upper…    0.867      1              
      4     AVAL incidenc…        conf.type    CI Type   normal   <fn>              
      5     AVAL incidenc…       conf.level  CI Confi…     0.95      1              
      6     AVAL incidenc… tot_person_years  Person-Y… 1404.417      1              
      7     AVAL incidenc…         n_events  Number o…      102      1              
      8     AVAL incidenc…      n_unique_id  Number o…      102      0              

# ard_incidence_rate(conf.type) works

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label      stat fmt_fn warning error
      1     AVAL incidenc…         estimate  Estimate…     0.605      1              
      2     AVAL incidenc…         conf.low  CI Lower…     0.498      1              
      3     AVAL incidenc…        conf.high  CI Upper…     0.735      1              
      4     AVAL incidenc…        conf.type    CI Type normal-l…   <fn>              
      5     AVAL incidenc…       conf.level  CI Confi…      0.95      1              
      6     AVAL incidenc… tot_person_years  Person-Y…     16853      1              
      7     AVAL incidenc…         n_events  Number o…       102      1              
      8     AVAL incidenc…      n_unique_id  Number o…       102      0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label  stat fmt_fn warning error
      1     AVAL incidenc…         estimate  Estimate… 0.605      1              
      2     AVAL incidenc…         conf.low  CI Lower… 0.493      1              
      3     AVAL incidenc…        conf.high  CI Upper… 0.735      1              
      4     AVAL incidenc…        conf.type    CI Type exact   <fn>              
      5     AVAL incidenc…       conf.level  CI Confi…  0.95      1              
      6     AVAL incidenc… tot_person_years  Person-Y… 16853      1              
      7     AVAL incidenc…         n_events  Number o…   102      1              
      8     AVAL incidenc…      n_unique_id  Number o…   102      0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label  stat fmt_fn warning error
      1     AVAL incidenc…         estimate  Estimate… 0.605      1              
      2     AVAL incidenc…         conf.low  CI Lower… 0.496      1              
      3     AVAL incidenc…        conf.high  CI Upper… 0.731      1              
      4     AVAL incidenc…        conf.type    CI Type  byar   <fn>              
      5     AVAL incidenc…       conf.level  CI Confi…  0.95      1              
      6     AVAL incidenc… tot_person_years  Person-Y… 16853      1              
      7     AVAL incidenc…         n_events  Number o…   102      1              
      8     AVAL incidenc…      n_unique_id  Number o…   102      0              

# ard_incidence_rate() errors are handled correctly

    Code
      res <- ard_incidence_rate(cards::ADTTE, AVAL, CNSR, USUBJID, conf.type = "standard")
    Condition
      Error in `ard_incidence_rate()`:
      ! `conf.type` must be one of "normal", "normal-log", "exact", or "byar", not "standard".

---

    Code
      res <- ard_incidence_rate(cards::ADTTE, AVAL, CNSR, USUBJID, units = "month")
    Condition
      Error in `ard_incidence_rate()`:
      ! `units` must be one of "years", "months", "weeks", or "days", not "month".
      i Did you mean "months"?

