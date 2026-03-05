use std::collections::{BTreeSet, HashSet};
use std::sync::LazyLock;

use mitex::{CmdShape, CommandSpecItem, EnvShape};
use mitex_spec_gen::DEFAULT_SPEC;

use crate::error::RenderError;

const TYPST_DEFAULT_PREAMBLE: &str = concat!(
    "#set page(width: auto, height: auto, margin: 0.1em, fill: none)\n",
    "#set text(top-edge: \"bounds\", bottom-edge: \"bounds\")"
);

static MITEX_ALIAS_PRELUDE: LazyLock<String> = LazyLock::new(build_mitex_alias_prelude);

// Build preludes
// This part of code is from `mitex-rs/mitex/crates/mitex-cli/main.rs`.
fn build_mitex_alias_prelude() -> String {
    let mut reserved = HashSet::new();
    reserved.extend(["and", "or", "in", "not"]);

    let mut alias_set = BTreeSet::new();
    for (_, cmd) in DEFAULT_SPEC.items() {
        let alias = match cmd {
            CommandSpecItem::Cmd(CmdShape {
                alias: Some(alias), ..
            }) => alias.as_str(),
            CommandSpecItem::Env(EnvShape {
                alias: Some(alias), ..
            }) => alias.as_str(),
            _ => continue,
        };

        if alias.is_empty() || !alias.chars().all(|c| c.is_ascii_alphanumeric()) {
            continue;
        }

        if reserved.contains(alias) {
            continue;
        }

        alias_set.insert(alias);
    }

    alias_set
        .into_iter()
        .map(|alias| format!(r#"#let {alias} = mitex-scope.at("{alias}", default: none)"#))
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn convert_latex_to_typst_source(latex_code: &str) -> Result<String, RenderError> {
    let converted = mitex::convert_math(latex_code, None)
        .map_err(|message| RenderError::MitexConversionFailed { message })?;

    Ok(format!(
        "{preamble}\n#import \"/specs/mod.typ\": mitex-scope\n{alias_prelude}\n$ {converted} $",
        preamble = TYPST_DEFAULT_PREAMBLE,
        alias_prelude = MITEX_ALIAS_PRELUDE.as_str(),
        converted = converted
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alias_prelude_contains_expected_aliases() {
        let prelude = MITEX_ALIAS_PRELUDE.as_str();

        assert!(
            prelude.contains("#let mitexsqrt = mitex-scope.at(\"mitexsqrt\", default: none)"),
            "mitexsqrt alias should be present"
        );
        assert!(
            prelude.contains("#let frac = mitex-scope.at(\"frac\", default: none)"),
            "frac alias should be present"
        );
    }

    #[test]
    fn test_convert_latex_to_typst_source_wraps_with_mitex_scope() {
        let result = convert_latex_to_typst_source(r#"\frac{1}{2}"#);

        let Ok(source) = result else {
            panic!("valid latex should convert successfully: {result:?}");
        };

        assert!(source.contains(r#"#import "/specs/mod.typ": mitex-scope"#));
        assert!(source.contains(r#"#let mitexsqrt = mitex-scope.at("mitexsqrt", default: none)"#));
        assert!(source.contains("$ frac(1 ,2 ) $"));
    }
}
