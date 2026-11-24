set unstable
set lists

export LC_CTYPE := "en_US.UTF-8"

alias fmt := format
alias doc := build
alias lint := check

air_cmd := if which("air") != "" { "air" } else if which("r-air") != "" { "r-air" } else { "" }
open_cmd := if os() == "macos" { "open" } else { "xdg-open" }

# List all available recipes.
default:
    just --list

# Format all source files with `air` and `cargo fmt`.
format:
    @if [ -z "{{ air_cmd }}" ]; then echo "Error: neither 'air' nor 'r-air' found." >&2; exit 1; fi
    @{{ air_cmd }} format .
    @cargo fmt --manifest-path src/rust/Cargo.toml --all

    @echo "✅ All files formatted!"

# Run all checks: `air`, `jarl`, `spell_check()`, and `cargo clippy`.
check:
    @if [ -z "{{ air_cmd }}" ]; then echo "Error: neither 'air' nor 'r-air' found." >&2; exit 1; fi
    @{{ air_cmd }} format . --check
    @jarl check .
    @Rscript -e "devtools::spell_check()"
    @cargo clippy --manifest-path src/rust/Cargo.toml --all-targets --all-features -- -D warnings

    @echo "✅ All checks passed!"

# Clean the Rust build artifacts.
clean:
    cargo clean --manifest-path src/rust/Cargo.toml

# Build the package and check it as CRAN would.
pkg-check:
    Rscript -e 'devtools::check(document = FALSE, remote = TRUE, error_on = "warning")'

# Build the package.
build:
    cd src && cargo run --quiet --manifest-path rust/Cargo.toml --bin document
    Rscript -e "devtools::document()"
    @echo "✅ Package built successfully!"

# Run all tests in R and Rust.
test:
    @TESTTHAT_CPUS=4 Rscript -e "devtools::test(reporter = 'summary')"
    @cargo test --quiet --manifest-path src/rust/Cargo.toml
    @echo "🎉 All tests passed!"

# Render all documents and build the website.
site:
    Rscript tools/build-site.R
    @{{ open_cmd }} docs/index.html || true

# Update the wordlist for spell checking.
update-wordlist:
    Rscript -e "spelling::update_wordlist(confirm = FALSE)"

# Generate the package icon.
icon:
    @Rscript "tools/icon.R"
    @echo "✅ Icon generated!"
