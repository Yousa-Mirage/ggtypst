
# Contributing to ggtypst

Thanks for your interest in contributing to `ggtypst`.

`ggtypst` is an R package with a Rust backend. It renders Typst text and
math to SVG, then inserts that output into `ggplot2` as annotations,
geoms, and theme elements. Most contributions touch both the R package
layer and the Rust rendering layer, so a good change usually starts with
understanding how those two halves fit together.

## Development setup

You will need:

- R \>= 4.2
- Rust \>= 1.89 with `cargo` and `rustc`
- `xz` available on the system
- recommended development packages such as `devtools`, `pkgdown`, and
  `testthat`

Install package dependencies in your usual R environment, then use the
project root as your working directory.

## Build model

`ggtypst` is not a pure R package. The package builds a Rust `staticlib`
under `src/rust/`, then links that library into the R package through
`extendr`.

At a high level, the pipeline is:

1.  R user-facing functions collect arguments and normalize them into
    Typst or MiTeX-backed math source.
2.  `extendr` forwards the request into Rust.
3.  Rust compiles Typst source in an in-memory world, renders the first
    page to SVG, and returns dimensions plus diagnostics.
4.  R converts the SVG into grobs and plugs those grobs into `ggplot2`.

That split is the key architectural idea of the package: R owns
user-facing API design and `ggplot2` integration; Rust owns rendering,
Typst world setup, and diagnostics emitted by the Typst compiler.

## Repository layout

- `R/`: exported functions, helpers, grob construction, diagnostics, and
  ggplot2 integration
- `src/rust/src/`: Rust renderer, MiTeX conversion, embedded Typst
  assets, and error translation
- `src/`: generated `rextendr` glue
- `tests/testthat/`: R tests
- `vignettes/`: pkgdown articles and long-form docs
- `inst/examples/`: showcase scripts used to render the PNG examples in
  `man/figures/`
- `tools/`: development scripts, including the site build wrapper

## Architecture overview

### R layer

The R API is organized around three feature families:

- `annotate_*()` for one-off plot annotations
- `geom_*()` for data-driven Typst labels
- `element_*()` for Typst-rendered theme text

The helper layer in `R/helper.R` centralizes input validation, alias
handling, size conversion, color normalization, and face normalization.
Prefer reusing those helpers instead of open-coding validation.

`R/zzz.R` performs runtime registration that cannot be expressed
statically, especially for `ggplot2` generics that are owned outside
this package.

### Rust layer

The Rust crate lives in `src/rust/` and currently exposes two exported
entry points:

- `rs_typst_svg()` compiles Typst and returns SVG output plus
  diagnostics
- `rs_convert_latex_to_typst()` converts LaTeX-style input through MiTeX

Important modules include:

- `world.rs`: builds the in-memory Typst world and serves embedded Typst
  files
- `render.rs`: compiles Typst documents and converts the first page to
  SVG
- `mitex.rs`: bridges LaTeX-style input through MiTeX
- `error.rs`: translates Rust-side failures into structured R errors
- `fonts.rs`: loads and caches embedded/system fonts for Typst rendering

The three embedded Typst files under `src/rust/specs/` provide the MiTeX
scope used when evaluating LaTeX-style formulas.

## Core development commands

The repository uses `just` as the main command runner:

- `just format` - run `r-air format .` and `cargo fmt`
- `just check` - run `jarl check .`, `devtools::spell_check()`, and
  `cargo clippy`
- `just document` - regenerate `rextendr` wrappers, `NAMESPACE`, and Rd
  files
- `just test` - run R tests plus Rust tests
- `just build-check` - run `R CMD check .`
- `just site` - rebuild `README.md`, community docs, and the pkgdown
  site
- `just render-showcases` - render the showcase PNG files in
  `man/figures/`

Useful direct commands while iterating:

- `Rscript -e "devtools::load_all('.')"`
- `Rscript -e "devtools::test(filter = 'geom-typst')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-element-typst.R')"`
- `cargo test --manifest-path src/rust/Cargo.toml`
- `cargo clippy --manifest-path src/rust/Cargo.toml`

## Documentation workflow

- `README.Rmd` is the source for `README.md`
- `.github/CONTRIBUTING.Rmd` is the source for `.github/CONTRIBUTING.md`
- `vignettes/` contains package articles such as the Get Started guide
- `tools/build-site.R` renders the R Markdown sources, hides agent-only
  docs, runs `pkgdown::init_site()`, then builds the site

If you edit any of those documentation sources, run `just site` before
opening or updating a documentation pull request.

## Generated files

Do not hand-edit generated files unless you are explicitly debugging
generation:

- `README.md`
- `.github/CONTRIBUTING.md`
- `NAMESPACE`
- `man/*.Rd`
- `R/extendr-wrappers.R`

Instead, edit their source files and regenerate outputs.

## Style expectations

### R

- use two-space indentation
- prefer explicit namespace qualification such as `ggplot2::`, `grid::`,
  and `cli::`
- use `snake_case` for functions and helpers
- prefer `cli::cli_abort()` for user-facing errors
- keep validation near the top of user-facing functions

### Rust

- use four-space indentation
- use `snake_case` for functions and modules
- keep `Result`-based error handling explicit
- do not introduce `unwrap()` or `expect()` in production code
- keep rendering logic in Rust modules, not in `extendr` entrypoints

### Tests

- add focused `testthat` coverage for R-facing behavior
- add or update Rust unit tests for rendering internals when relevant
- use snapshots only when output is expected to remain stable
- update showcase PNGs only when visual changes are intentional

## Performance notes

On Linux, the repository config prefers the `mold` linker for faster
Rust link steps during local development. That speeds up `load_all()`,
`test()`, and site builds when they trigger native recompilation.

The Rust portion may still start with many parallel compiler processes
and then settle into a slower final phase. That is expected: dependency
compilation is parallel, while the last link-heavy steps have much less
parallelism.

## Submitting changes

Before submitting a pull request:

1.  run the narrowest relevant tests locally
2.  regenerate docs if exports or roxygen comments changed
3.  rebuild the site if README, CONTRIBUTING, or vignettes changed
4.  keep commits focused on one logical change

Conventional commit prefixes such as `feat:`, `fix:`, `refactor:`,
`test:`, `doc:`, and `chore:` are used in this repository.

## Where to start

Good first contributions include:

- tightening argument validation and diagnostics
- adding tests for unsupported edge cases
- improving pkgdown docs and examples
- refining MiTeX/Typst interoperability behavior
- improving rendering consistency across annotate, geom, and element
  APIs

If you are unsure where a change belongs, open an issue or draft pull
request with your proposed approach first. That is especially helpful
for changes that touch public API behavior, rendering semantics, or the
Rust/R interface.
