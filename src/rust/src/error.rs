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

#[derive(Debug)]
pub enum RenderError {
    CompilationFailed { diagnostics: Vec<RenderDiagnostic> },
    NoPagesGenerated,
}

impl std::error::Error for RenderError {}

impl std::fmt::Display for RenderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RenderError::CompilationFailed { diagnostics } => {
                let summary = diagnostics
                    .iter()
                    .map(|diagnostic| {
                        let level = match diagnostic.severity {
                            Severity::Error => "error",
                            Severity::Warning => "warning",
                        };

                        if diagnostic.hints.is_empty() {
                            format!("[{level}] {}", diagnostic.message)
                        } else {
                            format!(
                                "[{level}] {} (hints: {})",
                                diagnostic.message,
                                diagnostic.hints.join("; ")
                            )
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" | ");

                write!(f, "Typst compilation failed: {summary}")
            }
            RenderError::NoPagesGenerated => write!(f, "Typst compilation produced no pages"),
        }
    }
}
