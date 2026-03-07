.onLoad <- function(libname, pkgname) {
  # Register the S7 merge method for the installed namespace
  S7::methods_register()

  # attach the S3 element_grob method for the namespaced S7 class name.
  registerS3method(
    "element_grob",
    paste0(pkgname, "::element_typst"),
    element_grob.element_typst,
    envir = asNamespace("ggplot2")
  )
}
