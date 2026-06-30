# annotate_math_mitex reports MiTeX conversion errors

    Code
      annotate_math_mitex("\\end{}", x = 2, y = 20)
    Condition
      Error in `annotate_math_mitex()`:
      ! Failed to convert LaTeX math in `annotate_math_mitex()`.
      i Input: "\\end{}"
      Caused by error in `convert_latex_to_typst()`:
      ! MiTeX conversion failed: error: error unexpected: ""

# annotate_math_mitex validates math-only face

    Code
      annotate_math_mitex("\\frac{1}{2}", x = 2, y = 20, face = "italic")
    Condition
      Error in `annotate_math_mitex()`:
      ! `face` for `annotate_math_mitex()` must be either "plain" or "bold".

# annotate_math_mitex requires trailing optional arguments to be named

    Code
      annotate_math_mitex("\\frac{1}{2}", 2, 20, TRUE)
    Condition
      Error in `annotate_math_mitex()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = TRUE
      i Did you forget to name an argument?

---

    Code
      annotate_math_mitex("\\frac{1}{2}", x = 2, y = 20, inlne = TRUE)
    Condition
      Error in `annotate_math_mitex()`:
      ! `...` must be empty.
      x Problematic argument:
      * inlne = TRUE

