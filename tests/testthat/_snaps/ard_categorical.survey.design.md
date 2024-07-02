# ard_categorical.survey.design() returns an error when variables have all NAs

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "row")
    Condition
      Error in `ard_categorical()`:
      ! Column "Class" is a factor with NA levels, which are not allowed.

---

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "column")
    Condition
      Error in `ard_categorical()`:
      ! Column "Class" is a factor with NA levels, which are not allowed.

---

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "cell")
    Condition
      Error in `ard_categorical()`:
      ! Column "Class" is a factor with NA levels, which are not allowed.

