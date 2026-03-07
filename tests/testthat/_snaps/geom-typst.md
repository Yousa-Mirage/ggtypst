# geom_typst converts mapped size according to size.unit

    Code
      geom_typst(label = "scale", size.unit = "px")
    Condition
      Error in `check_size_unit()`:
      ! `size.unit` must be one of "pt" or "mm".

# geom_typst validates face

    Code
      geom_typst(label = "scale", face = "oblique")
    Condition
      Error in `normalize_face()`:
      ! `face` must be one of "plain", "bold", "italic", or "bold.italic" (or numeric codes 1-4).

# geom_typst rejects position with nudge parameters

    Code
      geom_typst(label = "scale", position = "identity", nudge_x = 0.1)
    Condition
      Error in `geom_typst()`:
      ! You must specify either `position` or `nudge_x`/`nudge_y`, not both.

# geom_typst rejects duplicate face aliases

    Code
      geom_typst(label = "scale", face = "plain", fontface = "bold")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `face` and `fontface`.
      i Use `face`; `fontface` is an alias.

# geom_typst validates mapped face vectors before row rendering

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_typst()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error:
      ! Invalid `face` aesthetic in `geom_typst()`.
      x Problem in row 2 with value "oblique".
      i Supported values are "plain", "bold", "italic", or "bold.italic" (or numeric codes 1-4).

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

