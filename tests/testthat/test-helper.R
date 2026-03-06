test_that("check_single_string handles valid and invalid inputs", {
  expect_null(check_single_string(NULL, "x"))
  expect_equal(check_single_string("abc", "x"), "abc")

  expect_error(check_single_string(NULL, "x", allow_null = FALSE))
  expect_error(check_single_string(c("a", "b"), "x"))
  expect_error(check_single_string(1, "x"))
  expect_error(check_single_string(NA_character_, "x"))
})

test_that("check_positive_number validates numeric scalar", {
  expect_null(check_positive_number(NULL, "size"))
  expect_equal(check_positive_number(2, "size"), 2)

  expect_error(check_positive_number(NULL, "size", allow_null = FALSE))
  expect_error(check_positive_number(0, "size"))
  expect_error(check_positive_number(-1, "size"))
  expect_error(check_positive_number(Inf, "size"))
  expect_error(check_positive_number(c(1, 2), "size"))
})

test_that("check_bool validates logical scalar", {
  expect_null(check_bool(NULL, "inline"))
  expect_true(check_bool(TRUE, "inline"))
  expect_false(check_bool(FALSE, "inline"))

  expect_error(check_bool(NULL, "inline", allow_null = FALSE))
  expect_error(check_bool(1, "inline"))
  expect_error(check_bool(c(TRUE, FALSE), "inline"))
  expect_error(check_bool(NA, "inline"))
})

test_that("check_alpha validates alpha range", {
  expect_null(check_alpha(NULL))
  expect_equal(check_alpha(0), 0)
  expect_equal(check_alpha(1), 1)
  expect_equal(check_alpha(0.25), 0.25)

  expect_error(check_alpha(-0.1))
  expect_error(check_alpha(1.1))
  expect_error(check_alpha(NA_real_))
  expect_error(check_alpha(Inf))
  expect_error(check_alpha(c(0.1, 0.2)))
})

test_that("color_to_hex converts colors and combines alpha", {
  expect_equal(color_to_hex("red"), "#FF0000FF")
  expect_equal(color_to_hex("#33669980"), "#33669980")
  expect_equal(color_to_hex("#33669980", alpha = 0.5), "#33669940")
  expect_equal(color_to_hex("black", alpha = 0), "#00000000")
  expect_equal(color_to_hex("#eff1f5"), "#EFF1F5FF")
  expect_equal(color_to_hex("#8839ef", alpha = 0.5), "#8839EF80")
  expect_equal(color_to_hex(1), "#000000FF")

  expect_error(color_to_hex("not-a-color"), "invalid color name")
  expect_error(color_to_hex("eff1f5"), "invalid color name")
  expect_error(color_to_hex("#12345"), "invalid RGB")
  expect_error(color_to_hex(0), "numerical color")
})

test_that("format_typst_number returns compact, stable numeric strings", {
  expect_equal(format_typst_number(12), "12")
  expect_equal(format_typst_number(12.5), "12.5")
  expect_equal(format_typst_number(1 / 3), "0.333333")
  expect_equal(format_typst_number(0.000001), "0.000001")
  expect_equal(format_typst_number(1000000), "1000000")
})

test_that("convert_size_to_pt supports pt and mm units", {
  expect_null(convert_size_to_pt(NULL, "pt"))
  expect_equal(convert_size_to_pt(12, "pt"), 12)
  expect_equal(convert_size_to_pt(3.88, "mm"), 3.88 * ggplot2::.pt)

  expect_error(convert_size_to_pt(12, "px"), "size.unit")
  expect_error(convert_size_to_pt(0, "pt"), "positive finite number")
})

test_that("unwrap_math_dollar_delimiters unwraps only outer math delimiters", {
  expect_equal(unwrap_math_dollar_delimiters("x^2 + y^2"), "x^2 + y^2")
  expect_equal(unwrap_math_dollar_delimiters("$ x^2 + y^2 $"), "x^2 + y^2")
  expect_equal(unwrap_math_dollar_delimiters("$$ x^2 + y^2 $$"), "x^2 + y^2")
  expect_equal(
    unwrap_math_dollar_delimiters("\n  $$ x^2 + y^2 $$  \n"),
    "x^2 + y^2"
  )
})

test_that("as_typst_math_code always returns single-dollar wrapped math", {
  expect_math <- "$ x^2 + y^2 $"
  expect_equal(as_typst_math_code("x^2 + y^2"), expect_math)
  expect_equal(as_typst_math_code("$ x^2 + y^2 $"), expect_math)
  expect_equal(as_typst_math_code("$$ x^2 + y^2 $$"), expect_math)
})

test_that("as_typst_math_code supports inline mode", {
  expect_equal(as_typst_math_code("x^2 + y^2", inline = TRUE), "$x^2 + y^2$")
  expect_equal(as_typst_math_code("$ x^2 + y^2 $", inline = TRUE), "$x^2 + y^2$")
  expect_error(as_typst_math_code("x^2", inline = NA), "TRUE or FALSE")
})

test_that("as_typst_math_code process inline $ correctly", {
  expect_error(as_typst_math_code("$x$ + $y$"), "Invalid math input")
  expect_error(as_typst_math_code("$"), "Invalid math input")
  expect_error(as_typst_math_code(" $$ x $ y $$ "), "Invalid math input")

  expect_equal(as_typst_math_code(r"(x + \$y\$)"), "$ x + \\$y\\$ $")
})

test_that("as_latex_math_code normalizes outer delimiters", {
  expect_equal(as_latex_math_code("\\frac{1}{2}"), "\\frac{1}{2}")
  expect_equal(as_latex_math_code("$ \\frac{1}{2} $"), "\\frac{1}{2}")
  expect_equal(as_latex_math_code("$$ \\frac{1}{2} $$"), "\\frac{1}{2}")
  expect_equal(
    as_latex_math_code("\n $$ \\sqrt{3} $$ \n"),
    "\\sqrt{3}"
  )
})

test_that("as_latex_math_code rejects inline unescaped dollars", {
  expect_error(
    as_latex_math_code("\\$100 + $x$"),
    "Invalid math input"
  )
  expect_equal(as_latex_math_code(r"(\\$100 + x)"), r"(\\$100 + x)")
})
