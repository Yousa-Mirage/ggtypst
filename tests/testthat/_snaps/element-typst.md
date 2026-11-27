# element_typst() color alias works

    Code
      element_typst(colour = "red", color = "blue")
    Condition
      Error in `element_typst()`:
      ! Can't supply both `colour` and `color`.
      i Use `colour`; `color` is an alias.

# element_typst() fontface alias works

    Code
      element_typst(face = "plain", fontface = "bold")
    Condition
      Error in `element_typst()`:
      ! Can't supply both `face` and `fontface`.
      i Use `face`; `fontface` is an alias.

# element_typst() stores fontweight as a number

    Code
      element_typst(fontweight = 600, weight = 700)
    Condition
      Error in `element_typst()`:
      ! Can't supply both `fontweight` and `weight`.
      i Use `fontweight`; `weight` is an alias.

---

    Code
      element_typst(fontweight = 50)
    Condition
      Error in `element_typst()`:
      ! `fontweight` must be a whole number from 100 to 900 or one of "thin", "extralight", "light", "regular", "medium", "semibold", "bold", "extrabold", or "black".

# element_typst() size.unit validates input

    Code
      element_typst(size.unit = "inches")
    Condition
      Error in `element_typst()`:
      ! `size.unit` must be one of "pt" or "mm".

