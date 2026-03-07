# element_math_typst() aliases and validation work

    Code
      element_math_typst(size.unit = "inches")
    Condition
      Error in `check_size_unit()`:
      ! `size.unit` must be one of "pt" or "mm".

---

    Code
      element_math_typst(face = "italic")
    Condition
      Error in `element_math_typst()`:
      ! `face` for `element_math_typst()` must be either "plain" or "bold".

---

    Code
      element_math_typst(face = "plain", fontface = "bold")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `face` and `fontface`.
      i Use `face`; `fontface` is an alias.

---

    Code
      element_math_typst(colour = "red", color = "blue")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `colour` and `color`.
      i Use `colour`; `color` is an alias.

# element_grob.element_math_typst validates math-only face overrides

    Code
      element_grob(element_math_typst(), label = "x^2", face = "italic")
    Condition
      Error in `element_math_typst()`:
      ! `face` for `element_math_typst()` must be either "plain" or "bold".

