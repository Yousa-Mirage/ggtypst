# typst_fonts lists each family once by default

    Code
      typst_fonts(detail = NA)
    Condition
      Error in `typst_fonts()`:
      ! `detail` must be TRUE or FALSE.

---

    Code
      typst_fonts("libertinus", TRUE)
    Condition
      Error in `typst_fonts()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = TRUE
      i Did you forget to name an argument?

# typst_fonts filters and sorts fonts

    Code
      typst_fonts(1)
    Condition
      Error in `typst_fonts()`:
      ! `family` must be a single non-missing string.

# unknown family warnings suggest the name Typst uses

    Typst emitted warnings during rendering.
    ! unknown font family: newcomputermodern math
    i Did you mean "New Computer Modern Math"?
    i Run `ggtypst::typst_fonts()` to see the font families Typst can use.

