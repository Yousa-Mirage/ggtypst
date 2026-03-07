# element_math_mitex() aliases and validation work

    Code
      element_math_mitex(size.unit = "inches")
    Condition
      Error in `check_size_unit()`:
      ! `size.unit` must be one of "pt" or "mm".

---

    Code
      element_math_mitex(face = "italic")
    Condition
      Error in `element_math_mitex()`:
      ! `face` for `element_math_mitex()` must be either "plain" or "bold".

---

    Code
      element_math_mitex(face = "plain", fontface = "bold")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `face` and `fontface`.
      i Use `face`; `fontface` is an alias.

---

    Code
      element_math_mitex(colour = "red", color = "blue")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `colour` and `color`.
      i Use `colour`; `color` is an alias.

# element_grob.element_math_mitex validates math-only face overrides

    Code
      element_grob(element_math_mitex(), label = "\\alpha", face = "italic")
    Condition
      Error in `element_math_mitex()`:
      ! `face` for `element_math_mitex()` must be either "plain" or "bold".

# element_grob.element_math_mitex reports conversion errors

    Code
      element_grob(element_math_mitex(), label = "\\end{}")
    Condition
      Error in `element_math_mitex()`:
      ! Failed to convert a LaTeX math label in `element_math_mitex()`.
      i Label: "\\end{}"
      Caused by error in `convert_latex_to_typst()`:
      ! MiTeX conversion failed: error: error unexpected: ""

