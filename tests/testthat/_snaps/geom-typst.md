# geom_typst converts mapped size according to size.unit

    Code
      geom_typst(label = "scale", size.unit = "px")
    Condition
      Error in `check_size_unit()`:
      ! `size.unit` must be one of "pt" or "mm".

# geom_typst reports row and label context for render failures

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_typst()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 1st layer.
      Caused by error:
      ! Failed to render a Typst label in `geom_typst()`.
      x Problem in row 2.
      i Label: "BROKEN_LABEL_123 ["
      Caused by error in `typst_svg()`:
      ! Typst compilation failed
      x unclosed delimiter

