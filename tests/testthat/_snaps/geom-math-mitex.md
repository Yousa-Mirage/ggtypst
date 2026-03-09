# geom_math_mitex supports color alias

    Code
      geom_math_mitex(label = "\\frac{1}{2}", colour = "red", color = "blue")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `colour` and `color`.
      i Use `colour`; `color` is an alias.

# geom_math_mitex validates static math face

    Code
      geom_math_mitex(label = "\\frac{1}{2}", face = "italic")
    Condition
      Error in `geom_math_mitex()`:
      ! `face` for `geom_math_mitex()` must be either "plain" or "bold".

# geom_math_mitex validates mapped math face before rendering

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_math_mitex()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error:
      ! Invalid `face` aesthetic in `geom_math_mitex()`.
      x Problem in row 2 with value "italic".
      i Math labels only support "plain" and "bold".

# geom_math_mitex reports row and label context for conversion errors

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_math_mitex()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error:
      ! Failed to convert a LaTeX math label in `geom_math_mitex()`.
      x Problem in row 2.
      i Label: "\\end{}"
      Caused by error in `convert_latex_to_typst()`:
      ! MiTeX conversion failed: error: error unexpected: ""

# geom_math_mitex reports static label conversion errors

    Code
      geom_math_mitex(label = "\\end{}")
    Condition
      Error in `geom_math_mitex()`:
      ! Failed to convert the static `label` parameter in `geom_math_mitex()`.
      i Label: "\\end{}"
      Caused by error in `convert_latex_to_typst()`:
      ! MiTeX conversion failed: error: error unexpected: ""

