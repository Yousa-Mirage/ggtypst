use extendr_api::prelude::{list, List};
use typst::compile;
use typst::diag::Warned;
use typst::layout::PagedDocument;

use crate::error::{RenderDiagnostic, RenderError};
use crate::world::InMemoryWorld;

#[derive(Debug)]
pub struct RenderedSvg {
    pub svg: Vec<u8>,
    pub width_pt: f64,
    pub height_pt: f64,
}

impl RenderedSvg {
    pub fn to_r_list(&self) -> List {
        list!(
            svg = self.svg.clone(),
            width_pt = self.width_pt,
            height_pt = self.height_pt
        )
    }
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

        if width_pt <= 0.0 || height_pt <= 0.0 {
            return Err(RenderError::EmptySvg);
        }

        Ok(RenderedSvg {
            svg,
            width_pt,
            height_pt,
        })
    }
}
