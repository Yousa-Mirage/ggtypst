test_that("convert_latex_to_typst handles quote and backslash-like fragments safely", {
  special_cases <- c(
    r"(\text{a "b"})",
    r"(\left( x \right))",
    r"(\text{path to file})",
    r"(\begin{pmatrix}1&2\\3&4\end{pmatrix})"
  )

  for (latex_code in special_cases) {
    src <- convert_latex_to_typst(latex_code)
    expect_no_error(typst_svg(build_typst_source(src)))
  }
})

test_that("convert_latex_to_typst is stable under randomized valid formulas", {
  set.seed(20260305)

  atoms <- c(
    r"(x)",
    r"(y)",
    r"(\alpha)",
    r"(\beta)",
    r"(\frac{1}{2})",
    r"(\sqrt{x})",
    r"(\sum_{i=1}^{n} i)",
    r"(\int_0^1 x^2 \, dx)",
    r"(\left( x \right))",
    r"(\text{hello})",
    r"(\text{a "b"})",
    r"(\begin{cases}x & \text{if } x > 0 \\ 0 & \text{otherwise}\end{cases})",
    r"(\begin{pmatrix}1&2\\3&4\end{pmatrix})"
  )
  ops <- c(" + ", " - ", " = ")

  make_formula <- function() {
    n_terms <- sample(1:4, 1)
    parts <- sample(atoms, n_terms, replace = TRUE)
    if (n_terms == 1) {
      return(parts)
    }

    connectors <- sample(ops, n_terms - 1, replace = TRUE)
    out <- parts[1]
    for (i in seq_len(n_terms - 1)) {
      out <- paste0(out, connectors[i], parts[i + 1])
    }
    out
  }

  formulas <- replicate(80, make_formula())

  # Convert stress
  for (latex_code in formulas) {
    expect_no_error(convert_latex_to_typst(latex_code))
  }

  # Compile stress on a representative subset
  for (latex_code in formulas[1:25]) {
    src <- convert_latex_to_typst(latex_code)
    expect_no_error(typst_svg(build_typst_source(src)))
  }
})

test_that("convert_latex_to_typst supports inline mode", {
  src_display <- convert_latex_to_typst(r"(\frac{1}{2})", inline = FALSE)
  src_inline <- convert_latex_to_typst(r"(\frac{1}{2})", inline = TRUE)

  expect_match(
    src_display,
    r"(#eval("$ " + _ggtypst_mitex_expr + " $", scope: mitex-scope))",
    fixed = TRUE
  )
  expect_match(
    src_inline,
    r"(#eval("$" + _ggtypst_mitex_expr + "$", scope: mitex-scope))",
    fixed = TRUE
  )
})
