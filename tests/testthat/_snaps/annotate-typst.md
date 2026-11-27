# annotate_typst validates placement arguments before rendering

    Code
      annotate_typst("[", x = 3, y = 25, scale = 0)
    Condition
      Error in `annotate_typst()`:
      ! `scale` must be a single positive finite number.

---

    Code
      annotate_typst("[", x = 3, y = 25, hjust = Inf)
    Condition
      Error in `annotate_typst()`:
      ! `hjust` must be a single finite number.

---

    Code
      annotate_typst("[", x = 3, y = 25, vjust = Inf)
    Condition
      Error in `annotate_typst()`:
      ! `vjust` must be a single finite number.

# annotate_typst requires optional arguments after x and y to be named

    Code
      annotate_typst("A", 3, 25, 0, 1)
    Condition
      Error in `annotate_typst()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 0
      * ..2 = 1
      i Did you forget to name an argument?

---

    Code
      annotate_typst("A", x = 3, y = 25, famliy = "Arial")
    Condition
      Error in `annotate_typst()`:
      ! `...` must be empty.
      x Problematic argument:
      * famliy = "Arial"

# annotate_typst explains that it cannot read files

    Code
      annotate_typst("#image(\"logo.png\")", x = 3, y = 25)
    Condition
      Error in `annotate_typst()`:
      ! Typst compilation failed
      x file not found (searched at /logo.png)
      i ggtypst cannot read files.

# annotate_typst validates face

    Code
      annotate_typst("A", x = 3, y = 25, face = "oblique")
    Condition
      Error in `annotate_typst()`:
      ! `face` must be one of "plain", "bold", "italic", or "bold.italic" (or numeric codes 1-4).

# annotate_typst rejects duplicate alias arguments

    Code
      annotate_typst("A", x = 3, y = 25, color = "red", colour = "blue")
    Condition
      Error in `annotate_typst()`:
      ! Can't supply both `color` and `colour`.
      i Use `color`; `colour` is an alias.

---

    Code
      annotate_typst("A", x = 3, y = 25, face = "plain", fontface = "bold")
    Condition
      Error in `annotate_typst()`:
      ! Can't supply both `face` and `fontface`.
      i Use `face`; `fontface` is an alias.

---

    Code
      annotate_typst("A", x = 3, y = 25, fontweight = 600, weight = 700)
    Condition
      Error in `annotate_typst()`:
      ! Can't supply both `fontweight` and `weight`.
      i Use `fontweight`; `weight` is an alias.

# annotate_typst sets the font weight

    Code
      annotate_typst("A", x = 3, y = 25, fontweight = "heavy")
    Condition
      Error in `annotate_typst()`:
      ! `fontweight` must be a whole number from 100 to 900 or one of "thin", "extralight", "light", "regular", "medium", "semibold", "bold", "extrabold", or "black".

