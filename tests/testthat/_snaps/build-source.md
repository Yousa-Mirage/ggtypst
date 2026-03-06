# build_typst_source validates arguments

    Code
      build_typst_source(NULL)
    Condition
      Error in `check_single_string()`:
      ! `typst_code` must be a single non-missing string.

---

    Code
      build_typst_source("A", size = 0)
    Condition
      Error in `check_positive_number()`:
      ! `size` must be a single positive finite number.

---

    Code
      build_typst_source("A", alpha = 1.1)
    Condition
      Error in `check_alpha()`:
      ! `alpha` must be NULL or a single number in [0, 1].

---

    Code
      build_typst_source("A", color = "not-a-color")
    Condition
      Error in `grDevices::col2rgb()`:
      ! invalid color name 'not-a-color'

# convert_latex_to_typst reports mitex conversion errors

    Code
      convert_latex_to_typst("\\end{}")
    Condition
      Error in `convert_latex_to_typst()`:
      ! MiTeX conversion failed: error: error unexpected: ""

