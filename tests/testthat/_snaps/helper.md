# color_to_hex converts colors and combines alpha

    Code
      color_to_hex("not-a-color")
    Condition
      Error:
      ! `color` must be a colour name or a hex code such as "#66CCFF".
      Caused by error in `grDevices::col2rgb()`:
      ! invalid color name 'not-a-color'

---

    Code
      color_to_hex("eff1f5")
    Condition
      Error:
      ! `color` must be a colour name or a hex code such as "#66CCFF".
      Caused by error in `grDevices::col2rgb()`:
      ! invalid color name 'eff1f5'

---

    Code
      color_to_hex("#12345")
    Condition
      Error:
      ! `color` must be a colour name or a hex code such as "#66CCFF".
      Caused by error in `grDevices::col2rgb()`:
      ! invalid RGB specification

---

    Code
      color_to_hex(0)
    Condition
      Error:
      ! `color` must be a colour name or a hex code such as "#66CCFF".
      Caused by error in `grDevices::col2rgb()`:
      ! numerical color values must be positive

# convert_size_to_pt supports pt and mm units

    Code
      convert_size_to_pt(12, "px")
    Condition
      Error:
      ! `size.unit` must be one of "pt" or "mm".

---

    Code
      convert_size_to_pt(0, "pt")
    Condition
      Error:
      ! `size` must be a single positive finite number.

# resolve_arg_alias resolves aliases and rejects duplicates

    Code
      resolve_arg_alias("red", "blue", "color", "colour")
    Condition
      Error:
      ! Can't supply both `color` and `colour`.
      i Use `color`; `colour` is an alias.

# normalize_face normalizes supported face values

    Code
      normalize_face("oblique", "face")
    Condition
      Error:
      ! `face` must be one of "plain", "bold", "italic", or "bold.italic" (or numeric codes 1-4).

---

    Code
      normalize_face(5, "face")
    Condition
      Error:
      ! `face` must be one of "plain", "bold", "italic", or "bold.italic" (or numeric codes 1-4).

# normalize_fontweight accepts numbers and Typst weight names

    Code
      normalize_fontweight(50)
    Condition
      Error:
      ! `fontweight` must be a whole number from 100 to 900 or one of "thin", "extralight", "light", "regular", "medium", "semibold", "bold", "extrabold", or "black".

---

    Code
      normalize_fontweight(450.5)
    Condition
      Error:
      ! `fontweight` must be a whole number from 100 to 900 or one of "thin", "extralight", "light", "regular", "medium", "semibold", "bold", "extrabold", or "black".

---

    Code
      normalize_fontweight("heavyish")
    Condition
      Error:
      ! `fontweight` must be a whole number from 100 to 900 or one of "thin", "extralight", "light", "regular", "medium", "semibold", "bold", "extrabold", or "black".

# normalize_fontweight_values validates each mapped value

    Code
      normalize_fontweight_values(c(600, 50), "geom_typst", 1:2)
    Condition
      Error:
      ! Invalid `fontweight` aesthetic in `geom_typst()`.
      x Problem in row 2 with value "50".
      i `fontweight` must be a whole number from 100 to 900 or one of "thin", "extralight", "light", "regular", "medium", "semibold", "bold", "extrabold", or "black".

# as_typst_math_code supports inline mode

    Code
      as_typst_math_code("x^2", inline = NA)
    Condition
      Error:
      ! `inline` must be TRUE or FALSE.

# as_typst_math_code process inline $ correctly

    Code
      as_typst_math_code("$x$ + $y$")
    Condition
      Error:
      ! Invalid math input: Unexpected unescaped `$` found inside the code.
      x Offending argument: `typst_math_code`.
      i If you want to use a literal dollar sign, escape it as `\$`.

---

    Code
      as_typst_math_code("$")
    Condition
      Error:
      ! Invalid math input: Unexpected unescaped `$` found inside the code.
      x Offending argument: `typst_math_code`.
      i If you want to use a literal dollar sign, escape it as `\$`.

---

    Code
      as_typst_math_code(" $$ x $ y $$ ")
    Condition
      Error:
      ! Invalid math input: Unexpected unescaped `$` found inside the code.
      x Offending argument: `typst_math_code`.
      i If you want to use a literal dollar sign, escape it as `\$`.

# as_latex_math_code rejects inline unescaped dollars

    Code
      as_latex_math_code("\\$100 + $x$")
    Condition
      Error:
      ! Invalid math input: Unexpected unescaped `$` found inside the code.
      x Offending argument: `latex_math_code`.
      i If you want to use a literal dollar sign, escape it as `\$`.

