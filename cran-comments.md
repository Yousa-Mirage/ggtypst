## Test environments

- Local Linux (Arch Linux), R 4.5.2

## R CMD check results

- `R CMD build .`
- `R CMD check --as-cran --no-manual ggtypst_0.0.0.9000.tar.gz`
- Result: 0 errors | 0 package warnings | 2 notes

## Notes

- This package includes a Rust backend and vendors Rust crates in
  `src/rust/vendor.tar.xz` so CRAN/offline builds do not need network access.
- A separate Typst or LaTeX installation is not required at runtime.
- Installed size on this platform is 39.4 MB, driven mostly by the compiled
  Rust shared library and its bundled dependencies.
- The remaining notes are expected for this submission: it is a new package with
  large vendored components, and the build log reports platform-specific
  compiler hardening flags used by the local toolchain.
