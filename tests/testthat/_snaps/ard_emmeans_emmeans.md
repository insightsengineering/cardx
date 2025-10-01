# ard_emmeans_emmeans() works

    Code
      print(ard_emmeans_emmeans, columns = "all")
    Message
      {cards} data frame: 8 x 10
    Output
        group1 variable variable_level   context  stat_name stat_label      stat fmt_fun warning error
      1     am contrast      am0 - am1 emmeans_…   estimate  Mean Dif…      0.61    <fn>              
      2     am contrast      am0 - am1 emmeans_…  std.error  Standard…     0.229    <fn>              
      3     am contrast      am0 - am1 emmeans_…         df  Degrees …       Inf    <fn>              
      4     am contrast      am0 - am1 emmeans_…   conf.low  CI Lower…     0.162    <fn>              
      5     am contrast      am0 - am1 emmeans_…  conf.high  CI Upper…     1.059    <fn>              
      6     am contrast      am0 - am1 emmeans_…    p.value    p-value     0.008    <fn>              
      7     am contrast      am0 - am1 emmeans_… conf.level  CI Confi…      0.95    <fn>              
      8     am contrast      am0 - am1 emmeans_…     method     method Least-sq…    <fn>              

---

    Code
      print(ard_emmeans_mean_estimates, columns = "all")
    Message
      {cards} data frame: 16 x 10
    Output
         group1 variable variable_level   context  stat_name stat_label      stat fmt_fun warning error
      1      am contrast              0 emmeans_…   estimate       Mean     0.726    <fn>              
      2      am contrast              0 emmeans_…  std.error  Standard…     0.165    <fn>              
      3      am contrast              0 emmeans_…         df  Degrees …       Inf    <fn>              
      4      am contrast              0 emmeans_…          n          n        19    <fn>              
      5      am contrast              0 emmeans_…   conf.low  CI Lower…     0.402    <fn>              
      6      am contrast              0 emmeans_…  conf.high  CI Upper…      1.05    <fn>              
      7      am contrast              0 emmeans_… conf.level  CI Confi…      0.95    <fn>              
      8      am contrast              0 emmeans_…     method     method Least-sq…    <fn>              
      9      am contrast              1 emmeans_…   estimate       Mean     0.116    <fn>              
      10     am contrast              1 emmeans_…  std.error  Standard…     0.117    <fn>              
      11     am contrast              1 emmeans_…         df  Degrees …       Inf    <fn>              
      12     am contrast              1 emmeans_…          n          n        13    <fn>              
      13     am contrast              1 emmeans_…   conf.low  CI Lower…    -0.114    <fn>              
      14     am contrast              1 emmeans_…  conf.high  CI Upper…     0.346    <fn>              
      15     am contrast              1 emmeans_… conf.level  CI Confi…      0.95    <fn>              
      16     am contrast              1 emmeans_…     method     method Least-sq…    <fn>              

# ard_emmeans_emmeans() errors are returned correctly

    Code
      print(ard, columns = "all")
    Message
      {cards} data frame: 8 x 10
    Output
        group1 variable variable_level   context  stat_name stat_label stat fmt_fun warning     error
      1     am contrast                emmeans_…   estimate  Mean Dif…         <fn>         There wa…
      2     am contrast                emmeans_…  std.error  Standard…         <fn>         There wa…
      3     am contrast                emmeans_…         df  Degrees …         <fn>         There wa…
      4     am contrast                emmeans_…   conf.low  CI Lower…         <fn>         There wa…
      5     am contrast                emmeans_…  conf.high  CI Upper…         <fn>         There wa…
      6     am contrast                emmeans_…    p.value    p-value         <fn>         There wa…
      7     am contrast                emmeans_… conf.level  CI Confi…         <fn>         There wa…
      8     am contrast                emmeans_…     method     method         <fn>         There wa…

---

    "There was an error evaluating the model `glm(formula = vs ~ am + mpg, data = ., family = nothing)`\nCaused by error:\n! object 'nothing' not found"

