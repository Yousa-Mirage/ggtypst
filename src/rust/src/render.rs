use extendr_api::prelude::{list, List};
use typst::compile;
use typst::diag::Warned;
use typst::layout::{FrameItem, PagedDocument};

use crate::error::{diagnostics_to_r_list, RenderDiagnostic, RenderError};
use crate::world::InMemoryWorld;

#[derive(Debug)]
pub struct RenderedSvg {
    pub svg: Vec<u8>,
    pub width_pt: f64,
    pub height_pt: f64,
    pub warnings: Vec<RenderDiagnostic>,
}

impl RenderedSvg {
    pub fn to_r_list(&self) -> List {
        list!(
            svg = self.svg.clone(),
            width_pt = self.width_pt,
            height_pt = self.height_pt,
            warnings = diagnostics_to_r_list(&self.warnings)
        )
    }
}

impl InMemoryWorld {
    pub fn compile_to_svg(&self) -> Result<RenderedSvg, RenderError> {
        let Warned { output, warnings } = compile::<PagedDocument>(&self);

        let warnings: Vec<RenderDiagnostic> = warnings.iter().map(RenderDiagnostic::from).collect();

        let output = output.map_err(|diagnostics| RenderError::CompilationFailed {
            diagnostics: diagnostics.iter().map(RenderDiagnostic::from).collect(),
        })?;

        let first_page = output.pages.first().ok_or(RenderError::NoPagesGenerated)?;

        if !has_visual_content(&first_page.frame) {
            return Err(RenderError::EmptySvg);
        }

        let svg = typst_svg::svg(first_page).into_bytes();
        let width_pt = first_page.frame.width().to_pt();
        let height_pt = first_page.frame.height().to_pt();

        Ok(RenderedSvg {
            svg,
            width_pt,
            height_pt,
            warnings,
        })
    }
}

/// Check if there are any visual content items.
fn has_visual_content(frame: &typst::layout::Frame) -> bool {
    for (_, item) in frame.items() {
        match item {
            // If we encounter a nested Group, recursively check its internal frame
            FrameItem::Group(group) => {
                if has_visual_content(&group.frame) {
                    return true;
                }
            }
            // Once we encounter a Text, Shape, or Image item, we can conclude that the SVG will have visual content
            FrameItem::Text(..) | FrameItem::Shape(..) | FrameItem::Image(..) => {
                return true;
            }
            // Ignore all Meta tags, links, bookmarks, and other non-visible elements
            _ => {}
        }
    }
    false
}
