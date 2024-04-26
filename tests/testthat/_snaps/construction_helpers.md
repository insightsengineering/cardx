# construct_model() works

    Code
      dplyr::filter(as.data.frame(ard_regression(construct_model(x = dplyr::rename(
        mtcars, `M P G` = mpg), formula = reformulate2(c("M P G", "cyl"), response = "hp"),
      method = "lm"))), stat_name %in% c("term", "estimate", "p.value"))
    Output
        variable    context stat_name  stat_label        stat fmt_fn
      1    M P G regression      term        term     `M P G`   NULL
      2    M P G regression  estimate Coefficient   -2.774769      1
      3    M P G regression   p.value     p-value   0.2125285      1
      4      cyl regression      term        term         cyl   NULL
      5      cyl regression  estimate Coefficient    23.97863      1
      6      cyl regression   p.value     p-value 0.002814958      1

