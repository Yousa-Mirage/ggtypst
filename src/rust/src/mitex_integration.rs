use crate::error::RenderError;

pub fn convert_latex_to_typst(latex_code: &str) -> Result<String, RenderError> {
    mitex::convert_math(latex_code, None)
        .map_err(|message| RenderError::MitexConversionFailed { message })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_latex_to_typst_fragment_returns_raw_fragment() {
        let fragment = convert_latex_to_typst(r#"\frac{1}{2}"#)
            .expect("valid latex should convert successfully");

        assert_eq!(fragment, "frac(1 ,2 )");
        assert!(
            !fragment.contains("#import"),
            "fragment conversion should not include import prelude"
        );
    }
}
