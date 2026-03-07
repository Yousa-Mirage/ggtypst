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

---

    Code
      build_typst_source("A", color = "red", colour = "blue")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `color` and `colour`.
      i Use `color`; `colour` is an alias.

---

    Code
      build_typst_source("A", face = "oblique")
    Condition
      Error in `normalize_face()`:
      ! `face` must be one of "plain", "bold", "italic", or "bold.italic" (or numeric codes 1-4).

---

    Code
      build_typst_source("A", face = "plain", fontface = "bold")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `face` and `fontface`.
      i Use `face`; `fontface` is an alias.

# convert_latex_to_typst reports mitex conversion errors

    Code
      convert_latex_to_typst("\\end{}")
    Condition
      Error in `convert_latex_to_typst()`:
      ! MiTeX conversion failed: error: error unexpected: ""

