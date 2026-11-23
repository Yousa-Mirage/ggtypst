use extendr_api::prelude::*;

mod bounds;
mod error;
mod fonts;
mod mitex;
mod render;
mod scene;
mod world;

/// Compile a Typst document and describe its first page for drawing.
#[extendr]
fn rs_typst_render(typst_code: &str) -> List {
    let fonts = fonts::get_fonts();
    let world = world::InMemoryWorld::new(typst_code.to_string(), fonts);

    let result = world.render();

    // Like typst-cli's watch mode, drop entries unused in the last 10 renders,
    // so nearby repeated labels still reuse their results.
    typst::comemo::evict(10);

    match result {
        Ok(rendered) => rendered.into_r_list(),
        Err(err) => err.to_typst_error(),
    }
}

/// Data of an embedded font, to write it to a file that devices can load.
#[extendr]
fn rs_typst_font_data(store_index: i32) -> Robj {
    usize::try_from(store_index)
        .ok()
        .and_then(scene::font_data)
        .map_or_else(|| ().into_robj(), |data| Raw::from_bytes(&data).into_robj())
}

/// Convert LaTeX code to Typst code.
#[extendr]
fn rs_convert_latex_to_typst(latex_code: &str) -> Robj {
    match mitex::convert_latex_to_typst(latex_code) {
        Ok(typst_code) => r!(typst_code),
        Err(err) => err.to_typst_error().into_robj(),
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod ggtypst;
    fn rs_typst_render;
    fn rs_typst_font_data;
    fn rs_convert_latex_to_typst;
}
