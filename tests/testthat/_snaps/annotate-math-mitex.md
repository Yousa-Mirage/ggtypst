# annotate_math_mitex reports MiTeX conversion errors

    Code
      annotate_math_mitex("\\end{}", x = 2, y = 20)
    Condition
      Error in `annotate_math_mitex()`:
      ! Failed to convert LaTeX math in `annotate_math_mitex()`.
      i Input: "\\end{}"
      Caused by error in `convert_latex_to_typst()`:
      ! MiTeX conversion failed: error: error unexpected: ""

