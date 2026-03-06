# color_to_hex converts colors and combines alpha

    Code
      color_to_hex("not-a-color")
    Condition
      Error in `grDevices::col2rgb()`:
      ! invalid color name 'not-a-color'

---

    Code
      color_to_hex("eff1f5")
    Condition
      Error in `grDevices::col2rgb()`:
      ! invalid color name 'eff1f5'

---

    Code
      color_to_hex("#12345")
    Condition
      Error in `grDevices::col2rgb()`:
      ! invalid RGB specification

---

    Code
      color_to_hex(0)
    Condition
      Error in `grDevices::col2rgb()`:
      ! numerical color values must be positive

# convert_size_to_pt supports pt and mm units

    Code
      convert_size_to_pt(12, "px")
    Condition
      Error in `check_size_unit()`:
      ! `size.unit` must be one of "pt" or "mm".

---

    Code
      convert_size_to_pt(0, "pt")
    Condition
      Error in `check_positive_number()`:
      ! `size` must be a single positive finite number.

# as_typst_math_code supports inline mode

    Code
      as_typst_math_code("x^2", inline = NA)
    Condition
      Error in `check_bool()`:
      ! `inline` must be TRUE or FALSE.

# as_typst_math_code process inline $ correctly

    Code
      as_typst_math_code("$x$ + $y$")
    Condition
      Error in `validate_no_unescaped_dollar()`:
      ! Invalid math input: Unexpected unescaped `$` found inside the code.
      x Offending argument: `typst_math_code`.
      i If you want to use a literal dollar sign, escape it as `\$`.

---

    Code
      as_typst_math_code("$")
    Condition
      Error in `validate_no_unescaped_dollar()`:
      ! Invalid math input: Unexpected unescaped `$` found inside the code.
      x Offending argument: `typst_math_code`.
      i If you want to use a literal dollar sign, escape it as `\$`.

---

    Code
      as_typst_math_code(" $$ x $ y $$ ")
    Condition
      Error in `validate_no_unescaped_dollar()`:
      ! Invalid math input: Unexpected unescaped `$` found inside the code.
      x Offending argument: `typst_math_code`.
      i If you want to use a literal dollar sign, escape it as `\$`.

# as_latex_math_code rejects inline unescaped dollars

    Code
      as_latex_math_code("\\$100 + $x$")
    Condition
      Error in `validate_no_unescaped_dollar()`:
      ! Invalid math input: Unexpected unescaped `$` found inside the code.
      x Offending argument: `latex_math_code`.
      i If you want to use a literal dollar sign, escape it as `\$`.

