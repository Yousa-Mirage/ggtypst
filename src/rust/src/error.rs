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
}

impl std::error::Error for RenderError {}

impl RenderError {
    fn kind(&self) -> &'static str {
        match self {
            RenderError::CompilationFailed { .. } => "CompilationFailed",
            RenderError::NoPagesGenerated => "NoPagesGenerated",
            RenderError::EmptySvg => "EmptySvg",
        }
    }

    pub fn to_typst_error(&self) -> List {
        let diagnostics = match self {
            RenderError::CompilationFailed { diagnostics } => diagnostics_to_r_list(diagnostics),
            RenderError::NoPagesGenerated => List::new(0),
            RenderError::EmptySvg => List::new(0),
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
        }
    }
}

pub fn diagnostics_to_r_list(diagnostics: &[RenderDiagnostic]) -> List {
    List::from_values(diagnostics.iter().map(RenderDiagnostic::to_r_list))
}
