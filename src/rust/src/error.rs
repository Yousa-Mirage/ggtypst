use extendr_api::prelude::*;
use typst::diag::{Severity, SourceDiagnostic};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderDiagnostic {
    pub severity: Severity,
    pub message: String,
    pub hints: Vec<String>,
}

impl From<&SourceDiagnostic> for RenderDiagnostic {
    fn from(diagnostic: &SourceDiagnostic) -> Self {
        Self {
            severity: diagnostic.severity,
            message: diagnostic.message.to_string(),
            hints: diagnostic.hints.iter().map(ToString::to_string).collect(),
        }
    }
}

impl RenderDiagnostic {
    fn severity_label(&self) -> &'static str {
        match self.severity {
            Severity::Error => "Error",
            Severity::Warning => "Warning",
        }
    }

    fn to_r_list(&self) -> List {
        list!(
            severity = self.severity_label(),
            message = self.message.clone(),
            hints = self.hints.clone()
        )
    }
}

#[derive(Debug)]
pub enum RenderError {
    CompilationFailed { diagnostics: Vec<RenderDiagnostic> },
    NoPagesGenerated,
    EmptySvg,
    MitexConversionFailed { message: String },
}

impl std::error::Error for RenderError {}

impl RenderError {
    fn kind(&self) -> &'static str {
        match self {
            RenderError::CompilationFailed { .. } => "CompilationFailed",
            RenderError::NoPagesGenerated => "NoPagesGenerated",
            RenderError::EmptySvg => "EmptySvg",
            RenderError::MitexConversionFailed { .. } => "MitexConversionFailed",
        }
    }

    pub fn to_typst_error(&self) -> List {
        let diagnostics = match self {
            RenderError::CompilationFailed { diagnostics } => diagnostics_to_r_list(diagnostics),
            RenderError::NoPagesGenerated => List::new(0),
            RenderError::EmptySvg => List::new(0),
            RenderError::MitexConversionFailed { .. } => List::new(0),
        };

        let mut err = list!(
            kind = self.kind(),
            message = self.to_string(),
            diagnostics = diagnostics
        );

        #[allow(clippy::unwrap_used)]
        err.set_class(&["typst_error", "list"]).unwrap();
        err
    }
}

impl std::fmt::Display for RenderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RenderError::CompilationFailed { .. } => write!(f, "Typst compilation failed"),
            RenderError::NoPagesGenerated => write!(f, "Typst compilation produced no pages"),
            RenderError::EmptySvg => write!(f, "Typst rendered an empty SVG"),
            RenderError::MitexConversionFailed { message } => {
                write!(f, "MiTeX conversion failed: {message}")
            }
        }
    }
}

pub fn diagnostics_to_r_list(diagnostics: &[RenderDiagnostic]) -> List {
    List::from_values(diagnostics.iter().map(RenderDiagnostic::to_r_list))
}

#[cfg(test)]
mod tests {
    use super::*;
    use typst::syntax::Span;

    #[test]
    fn test_render_diagnostic_from_source_diagnostic_preserves_fields() {
        let mut source = SourceDiagnostic::warning(Span::detached(), "missing font family");
        source.hint("install the requested font");

        let rendered = RenderDiagnostic::from(&source);

        assert_eq!(rendered.severity, Severity::Warning);
        assert_eq!(rendered.message, "missing font family");
        assert_eq!(rendered.hints, vec!["install the requested font"]);
    }

    #[test]
    fn test_render_error_display_messages() {
        let errors = [
            (
                RenderError::CompilationFailed {
                    diagnostics: vec![],
                },
                "Typst compilation failed",
            ),
            (
                RenderError::NoPagesGenerated,
                "Typst compilation produced no pages",
            ),
            (RenderError::EmptySvg, "Typst rendered an empty SVG"),
            (
                RenderError::MitexConversionFailed {
                    message: "unknown command".to_string(),
                },
                "MiTeX conversion failed: unknown command",
            ),
        ];

        for (error, expected_message) in errors {
            assert_eq!(error.to_string(), expected_message);
        }
    }

    #[test]
    fn test_render_error_kind_values() {
        assert_eq!(
            RenderError::CompilationFailed {
                diagnostics: vec![],
            }
            .kind(),
            "CompilationFailed"
        );
        assert_eq!(RenderError::NoPagesGenerated.kind(), "NoPagesGenerated");
        assert_eq!(RenderError::EmptySvg.kind(), "EmptySvg");
        assert_eq!(
            RenderError::MitexConversionFailed {
                message: "x".to_string(),
            }
            .kind(),
            "MitexConversionFailed"
        );
    }
}
