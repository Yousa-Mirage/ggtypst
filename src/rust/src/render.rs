use crate::world::InMemoryWorld;
use typst::compile;
use typst::{diag::Warned, layout::PagedDocument};

#[derive(Debug)]
pub struct RenderedSvg {
    pub svg: Vec<u8>,
    pub width_pt: f64,
    pub height_pt: f64,
    // pub warnings: Vec<String>,
}

impl InMemoryWorld {
    pub fn compile_to_svg(&self) -> RenderedSvg {
        let Warned { output, warnings } = compile::<PagedDocument>(&self);
        let output = output.unwrap();
        let first_page = output.pages.first().unwrap();

        let svg = typst_svg::svg(first_page).into_bytes();
        let width_pt = first_page.frame.width().to_pt();
        let height_pt = first_page.frame.height().to_pt();

        RenderedSvg {
            svg,
            width_pt,
            height_pt,
        }
    }
}
