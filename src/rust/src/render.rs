use crate::error::{RenderDiagnostic, RenderError};
use crate::world::InMemoryWorld;

use typst::compile;
use typst::diag::Warned;
use typst::layout::PagedDocument;

#[derive(Debug)]
pub struct RenderedSvg {
    pub svg: Vec<u8>,
    pub width_pt: f64,
    pub height_pt: f64,
}

impl InMemoryWorld {
    pub fn compile_to_svg(&self) -> Result<RenderedSvg, RenderError> {
        let Warned {
            output,
            warnings: _,
        } = compile::<PagedDocument>(&self);
        let output = output.map_err(|diagnostics| RenderError::CompilationFailed {
            diagnostics: diagnostics.iter().map(RenderDiagnostic::from).collect(),
        })?;

        let first_page = output.pages.first().ok_or(RenderError::NoPagesGenerated)?;

        let svg = typst_svg::svg(first_page).into_bytes();
        let width_pt = first_page.frame.width().to_pt();
        let height_pt = first_page.frame.height().to_pt();

        Ok(RenderedSvg {
            svg,
            width_pt,
            height_pt,
        })
    }
}
