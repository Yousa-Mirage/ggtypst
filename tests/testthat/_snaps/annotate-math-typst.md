# annotate_math_typst validates math-only face

    Code
      annotate_math_typst("x^2", x = 2, y = 20, face = "italic")
    Condition
      Error in `annotate_math_typst()`:
      ! `face` for `annotate_math_typst()` must be either "plain" or "bold".

# annotate_math_typst requires trailing optional arguments to be named

    Code
      annotate_math_typst("x^2", 2, 20, TRUE)
    Condition
      Error in `annotate_math_typst()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = TRUE
      i Did you forget to name an argument?

---

    Code
      annotate_math_typst("x^2", x = 2, y = 20, inlne = TRUE)
    Condition
      Error in `annotate_math_typst()`:
      ! `...` must be empty.
      x Problematic argument:
      * inlne = TRUE

