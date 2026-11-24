set unstable
set lists

alias fmt := format
alias doc := document
alias lint := check

air_cmd := if which("air") != "" { "air" } else if which("r-air") != "" { "r-air" } else { "" }
open_cmd := if os() == "macos" { "open" } else { "xdg-open" }

default:
    just --list

format:
    @if [ -z "{{ air_cmd }}" ]; then echo "Error: neither 'air' nor 'r-air' found." >&2; exit 1; fi
    @{{ air_cmd }} format .
    @cargo fmt --manifest-path src/rust/Cargo.toml --all

    @echo "✅ All files formatted!"

check:
    @jarl check .
    @Rscript -e "devtools::spell_check()"
    @cargo clippy --manifest-path src/rust/Cargo.toml --all-targets --all-features -- -D warnings

    @echo "✅ All checks passed!"

clean:
    cargo clean --manifest-path src/rust/Cargo.toml

pkg-check:
    R CMD check .

document:
    cd src && cargo run --quiet --manifest-path rust/Cargo.toml --bin document
    Rscript -e "devtools::document()"

test:
    @TESTTHAT_CPUS=4 Rscript -e "devtools::test(reporter = 'summary')"
    @cargo test --quiet --manifest-path src/rust/Cargo.toml
    @echo "🎉 All tests passed!"

site:
    Rscript tools/build-site.R
    @{{ open_cmd }} docs/index.html || true

update-wordlist:
    Rscript -e "spelling::update_wordlist(confirm = FALSE)"

icon:
    @Rscript "tools/icon.R"
    @echo "✅ Icon generated!"
