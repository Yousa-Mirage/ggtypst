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

