# element_typst() color alias works

    Code
      element_typst(colour = "red", color = "blue")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `colour` and `color`.
      i Use `colour`; `color` is an alias.

# element_typst() fontface alias works

    Code
      element_typst(face = "plain", fontface = "bold")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `face` and `fontface`.
      i Use `face`; `fontface` is an alias.

# element_typst() size.unit validates input

    Code
      element_typst(size.unit = "inches")
    Condition
      Error in `check_size_unit()`:
      ! `size.unit` must be one of "pt" or "mm".

