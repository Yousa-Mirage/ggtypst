# annotate_typst validates placement arguments before rendering

    Code
      annotate_typst("[", x = 3, y = 25, scale = 0)
    Condition
      Error in `check_positive_number()`:
      ! `scale` must be a single positive finite number.

---

    Code
      annotate_typst("[", x = 3, y = 25, hjust = Inf)
    Condition
      Error in `check_number()`:
      ! `hjust` must be a single finite number.

---

    Code
      annotate_typst("[", x = 3, y = 25, vjust = Inf)
    Condition
      Error in `check_number()`:
      ! `vjust` must be a single finite number.

# annotate_typst validates face

    Code
      annotate_typst("A", x = 3, y = 25, face = "oblique")
    Condition
      Error in `normalize_face()`:
      ! `face` must be one of "plain", "bold", "italic", or "bold.italic" (or numeric codes 1-4).

# annotate_typst rejects duplicate alias arguments

    Code
      annotate_typst("A", x = 3, y = 25, color = "red", colour = "blue")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `color` and `colour`.
      i Use `color`; `colour` is an alias.

---

    Code
      annotate_typst("A", x = 3, y = 25, face = "plain", fontface = "bold")
    Condition
      Error in `resolve_arg_alias()`:
      ! Can't supply both `face` and `fontface`.
      i Use `face`; `fontface` is an alias.

