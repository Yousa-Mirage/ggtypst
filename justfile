alias fmt := format
alias doc := document

default:
    just --list

format:
    r-air format .
    cargo fmt --manifest-path src/rust/Cargo.toml

check:
    jarl check .
    cargo clippy --manifest-path src/rust/Cargo.toml

clean:
    cargo clean --manifest-path src/rust/Cargo.toml

build-check:
    R CMD check .

document:
    Rscript -e "rextendr::document()"

test:
    Rscript -e "devtools::test()"
    cargo test --manifest-path src/rust/Cargo.toml