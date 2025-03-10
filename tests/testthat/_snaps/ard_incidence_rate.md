# ard_incidence_rate() works

    Code
      res
    Message
      {cards} data frame: 9 x 5
    Output
        variable   context        stat_name stat_label      stat
      1     AVAL incidenc…         estimate   estimate     0.605
      2     AVAL incidenc…   estimate_label  estimate… Estimate…
      3     AVAL incidenc…         conf.low  CI Lower…     0.488
      4     AVAL incidenc…        conf.high  CI Upper…     0.723
      5     AVAL incidenc…        conf.type    CI Type    normal
      6     AVAL incidenc…       conf.level  CI Confi…      0.95
      7     AVAL incidenc… tot_person_years  Person-Y…     16853
      8     AVAL incidenc…         n_events  Number o…       102
      9     AVAL incidenc…      n_unique_id  Number o…       102

---

    Code
      res
    Message
      {cards} data frame: 9 x 5
    Output
        variable   context        stat_name stat_label      stat
      1     AVAL incidenc…         estimate   estimate     0.726
      2     AVAL incidenc…   estimate_label  estimate… Estimate…
      3     AVAL incidenc…         conf.low  CI Lower…     0.585
      4     AVAL incidenc…        conf.high  CI Upper…     0.867
      5     AVAL incidenc…        conf.type    CI Type    normal
      6     AVAL incidenc…       conf.level  CI Confi…      0.95
      7     AVAL incidenc… tot_person_years  Person-Y…  1404.417
      8     AVAL incidenc…         n_events  Number o…       102
      9     AVAL incidenc…      n_unique_id  Number o…       102

# ard_incidence_rate(conf.type) works

    Code
      res
    Message
      {cards} data frame: 9 x 5
    Output
        variable   context        stat_name stat_label      stat
      1     AVAL incidenc…         estimate   estimate     0.605
      2     AVAL incidenc…   estimate_label  estimate… Estimate…
      3     AVAL incidenc…         conf.low  CI Lower…     0.498
      4     AVAL incidenc…        conf.high  CI Upper…     0.735
      5     AVAL incidenc…        conf.type    CI Type normal-l…
      6     AVAL incidenc…       conf.level  CI Confi…      0.95
      7     AVAL incidenc… tot_person_years  Person-Y…     16853
      8     AVAL incidenc…         n_events  Number o…       102
      9     AVAL incidenc…      n_unique_id  Number o…       102

---

    Code
      res
    Message
      {cards} data frame: 9 x 5
    Output
        variable   context        stat_name stat_label      stat
      1     AVAL incidenc…         estimate   estimate     0.605
      2     AVAL incidenc…   estimate_label  estimate… Estimate…
      3     AVAL incidenc…         conf.low  CI Lower…     0.493
      4     AVAL incidenc…        conf.high  CI Upper…     0.735
      5     AVAL incidenc…        conf.type    CI Type     exact
      6     AVAL incidenc…       conf.level  CI Confi…      0.95
      7     AVAL incidenc… tot_person_years  Person-Y…     16853
      8     AVAL incidenc…         n_events  Number o…       102
      9     AVAL incidenc…      n_unique_id  Number o…       102

---

    Code
      res
    Message
      {cards} data frame: 9 x 5
    Output
        variable   context        stat_name stat_label      stat
      1     AVAL incidenc…         estimate   estimate     0.605
      2     AVAL incidenc…   estimate_label  estimate… Estimate…
      3     AVAL incidenc…         conf.low  CI Lower…     0.496
      4     AVAL incidenc…        conf.high  CI Upper…     0.731
      5     AVAL incidenc…        conf.type    CI Type      byar
      6     AVAL incidenc…       conf.level  CI Confi…      0.95
      7     AVAL incidenc… tot_person_years  Person-Y…     16853
      8     AVAL incidenc…         n_events  Number o…       102
      9     AVAL incidenc…      n_unique_id  Number o…       102

# ard_incidence_rate() errors are handled correctly

    Code
      res <- ard_incidence_rate(cards::ADTTE, AVAL, CNSR, USUBJID, conf.type = "standard")
    Condition
      Error in `ard_incidence_rate()`:
      ! The `conf.type` argument is "standard" but must be one of "normal", "normal-log", "exact", and "byar".

---

    Code
      res <- ard_incidence_rate(cards::ADTTE, AVAL, CNSR, USUBJID, var_unit = "months")
    Condition
      Error in `ard_incidence_rate()`:
      ! The `var_unit` argument is "months" but must be one of "day", "week", "month", and "year".

