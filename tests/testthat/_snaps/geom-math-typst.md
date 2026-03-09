# geom_math_typst supports color alias

    Code
      geom_math_typst(label = "x^2", colour = "red", color = "blue")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `colour` and `color`.
      i Use `colour`; `color` is an alias.

# geom_math_typst validates static math face

    Code
      geom_math_typst(label = "x^2", face = "italic")
    Condition
      Error in `geom_math_typst()`:
      ! `face` for `geom_math_typst()` must be either "plain" or "bold".

# geom_math_typst validates mapped math face before rendering

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_math_typst()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error:
      ! Invalid `face` aesthetic in `geom_math_typst()`.
      x Problem in row 2 with value "italic".
      i Math labels only support "plain" and "bold".

# geom_math_typst reports row and label context for invalid math

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_math_typst()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error:
      ! Failed to normalize a Typst math label in `geom_math_typst()`.
      x Problem in row 2.
      i Label: "$x$ + $y$"
      Caused by error in `validate_no_unescaped_dollar()`:
      ! Invalid math input: Unexpected unescaped `$` found inside the code.
      x Offending argument: `typst_math_code`.
      i If you want to use a literal dollar sign, escape it as `\$`.

