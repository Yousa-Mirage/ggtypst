alias fmt := format
alias doc := document

default:
    just --list

format:
    r-air format .
    cargo fmt --manifest-path src/rust/Cargo.toml

check:
    jarl check .
    Rscript -e "devtools::spell_check()"
    cargo clippy --manifest-path src/rust/Cargo.toml

clean:
    cargo clean --manifest-path src/rust/Cargo.toml

build-check:
    R CMD check .

document:
    Rscript -e "rextendr::document()"

test:
    TESTTHAT_CPUS=4 Rscript -e "devtools::test(reporter = 'summary')"
    cargo test --quiet --manifest-path src/rust/Cargo.toml

site:
    Rscript tools/build-site.R
    @xdg-open docs/index.html || true

update-wordlist:
    Rscript -e "spelling::update_wordlist(confirm = FALSE)"

icon:
    @Rscript "pkgdown/generate-icon.R"
