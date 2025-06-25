# ard_emmeans_mean_difference() works

    Code
      print(ard_emmeans_mean_difference, columns = "all")
    Message
      {cards} data frame: 8 x 10
    Output
        group1 variable variable_level   context  stat_name stat_label      stat fmt_fun warning error
      1     am contrast      am0 - am1 emmeans_…   estimate  Mean Dif…      0.61       1              
      2     am contrast      am0 - am1 emmeans_…  std.error  std.error     0.229       1              
      3     am contrast      am0 - am1 emmeans_…         df         df       Inf       1              
      4     am contrast      am0 - am1 emmeans_…   conf.low  CI Lower…     0.162       1              
      5     am contrast      am0 - am1 emmeans_…  conf.high  CI Upper…     1.059       1              
      6     am contrast      am0 - am1 emmeans_…    p.value    p-value     0.008       1              
      7     am contrast      am0 - am1 emmeans_… conf.level  CI Confi…      0.95       1              
      8     am contrast      am0 - am1 emmeans_…     method     method Least-sq…    <fn>              

# ard_emmeans_mean_difference() errors are returned correctly

    Code
      print(ard, columns = "all")
    Message
      {cards} data frame: 8 x 10
    Output
        group1 variable variable_level   context  stat_name stat_label stat fmt_fun warning     error
      1     am contrast             NA emmeans_…   estimate  Mean Dif…         <fn>         There wa…
      2     am contrast             NA emmeans_…  std.error  std.error         <fn>         There wa…
      3     am contrast             NA emmeans_…         df         df         <fn>         There wa…
      4     am contrast             NA emmeans_…   conf.low  CI Lower…         <fn>         There wa…
      5     am contrast             NA emmeans_…  conf.high  CI Upper…         <fn>         There wa…
      6     am contrast             NA emmeans_…    p.value    p-value         <fn>         There wa…
      7     am contrast             NA emmeans_… conf.level  CI Confi…         <fn>         There wa…
      8     am contrast             NA emmeans_…     method     method         <fn>         There wa…

---

    "There was an error evaluating the model `glm(formula = vs ~ am + mpg, data = ., family = nothing)`\nCaused by error:\n! object 'nothing' not found"

