.onLoad <- function(libname, pkgname) {
  # Register the S7 merge method at load time because ggplot2 owns the generic.
  merge_element_generic <- utils::getFromNamespace("merge_element", "ggplot2")

  S7::method(merge_element_generic, list(element_typst_class, S7::class_any)) <- merge_element_element_typst

  # attach the S3 element_grob method for the namespaced S7 class name.
  registerS3method(
    "element_grob",
    paste0(pkgname, "::element_typst"),
    element_grob.element_typst,
    envir = asNamespace("ggplot2")
  )

  registerS3method(
    "element_grob",
    paste0(pkgname, "::element_math_typst"),
    element_grob.element_math_typst,
    envir = asNamespace("ggplot2")
  )

  registerS3method(
    "element_grob",
    paste0(pkgname, "::element_math_mitex"),
    element_grob.element_math_mitex,
    envir = asNamespace("ggplot2")
  )
}
