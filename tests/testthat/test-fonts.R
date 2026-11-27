test_that("typst_fonts lists each family once by default", {
  fonts <- typst_fonts()

  expect_s3_class(fonts, "data.frame")
  expect_named(fonts, c("family", "variable", "math"))
  expect_false(anyDuplicated(fonts$family) > 0)
  expect_false(is.unsorted(tolower(fonts$family)))
  expect_identical(rownames(fonts), as.character(seq_len(nrow(fonts))))

  built_in <- fonts[
    match(
      c("Libertinus Serif", "New Computer Modern Math"),
      fonts$family
    ),
  ]
  expect_identical(built_in$math, c(FALSE, TRUE))
  expect_identical(built_in$variable, c(FALSE, FALSE))

  expect_identical(
    typst_fonts("^libertinus serif$")$family,
    "Libertinus Serif"
  )
  expect_snapshot(
    typst_fonts(detail = NA),
    error = TRUE
  )

  expect_snapshot(
    typst_fonts("libertinus", TRUE),
    error = TRUE
  )
})

test_that("typst_fonts lists each font with detail = TRUE", {
  fonts <- typst_fonts(detail = TRUE)

  expect_s3_class(fonts, "data.frame")
  expect_named(
    fonts,
    c("family", "weight", "style", "stretch", "variable", "math")
  )
  expect_type(fonts$weight, "integer")
  expect_type(fonts$math, "logical")

  # A font both built in and installed is listed once.
  expect_false(anyDuplicated(fonts) > 0)

  libertinus <- fonts[fonts$family == "Libertinus Serif", ]
  expect_setequal(
    paste(libertinus$weight, libertinus$style),
    paste(c(400, 600, 700), rep(c("normal", "italic"), each = 3))
  )
  expect_false(any(libertinus$math))

  math <- fonts[fonts$family == "New Computer Modern Math", ]
  expect_true(all(math$math))
  expect_true(all(c(400, 450, 700) %in% math$weight))
})

test_that("typst_fonts filters and sorts fonts", {
  fonts <- typst_fonts("^libertinus serif$", detail = TRUE)

  expect_true(all(fonts$family == "Libertinus Serif"))
  expect_identical(rownames(fonts), as.character(seq_len(nrow(fonts))))

  expect_false(is.unsorted(fonts$style != "normal"))
  upright <- fonts$weight[fonts$style == "normal"]
  expect_false(is.unsorted(upright))

  expect_identical(nrow(typst_fonts("^no such family$")), 0L)
  expect_snapshot(typst_fonts(1), error = TRUE)
})

test_that("unknown family warnings suggest the name Typst uses", {
  # Windows reports this font as "NewComputerModern Math".
  expect_snapshot_warning(
    typst_render(build_typst_source("x", family = "NewComputerModern Math"))
  )

  expect_identical(
    unknown_family_hints(list(list(
      message = "unknown font family: libertinusserif"
    )))[[1]],
    'Did you mean {.val Libertinus Serif}?'
  )

  expect_identical(
    unknown_family_hints(list(list(message = "something else"))),
    character()
  )
})
